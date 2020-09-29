* 6309 Forth based on my 1984/07/16 TRS-80 Model 1 level II ROM implementation.
*
* This work is dedicated to Alain Pinaud, author of "Programmer en Forth"
* published by Editions du P.S.I. in 1983--i.e. my virtual mentor in the field.
* Special thanks go William Astle for his fantastic LW Tools utility set and
* to Robert Lipe for his advice on serial line handling. Apache subversion
* and the sdiff utility also were on my side all the time.
*
* Also credited for their help: Justin Poirier (seminal HW design), Daniel
* Tufvesson (CompactFlash interface), Peter Forth (FB alias),
* Paul E. Bennett and Michel Jean.
*
* This is a native Forth. Not a threaded interpretive implementation.
* Worth noticing is the fact that the return stack does not hold return
* addresses at all. All what is stored there is loop indexes and control
* structures jump addresses. Actual return addresses are kept in the system
* stack. All in all, this is the result of the work of a 19 year old guy,
* heavily revisited 35 years later. Some quirks remain that prevent me
* from offering the full required set--not to mention the choice of using
* an 8K EEPROM. The downside of this implementation is that the generated
* code is about 30% larger than a threaded interpretive implementation
* would be. So it goes...
*
* The code generated is limited to a very small instruction set:
*
* LDXOPC	$8E	LDX (immediate)
* JMPOPC	$7E	JMP (extended)
* JSROPC	$BD	JSR (extended)
* RTSOPC	$39	RTS (inherent)
* BCSOPC	$2503	BCS *+5	(relative) Used in LOOP, +LOOP
* BNEOPC	$2603	BNE *+5	(relative) Used in IF, UNTIL
*
* On error, the system stack pointer is reset. The return stack also is 
* but the data stack will be in the same state as when the error occurred.
* ABORT and QUIT enforce their own 79-STANDARD behaviour.
*
* The 79-STANDARD Forth specification can be downloaded from
* https://www.complang.tuwien.ac.at/forth/fth79std/FORTH-79.TXT
*
* Additionally, this implementation provides a few FORTH-83 words. Those
* words are CMOVE> and RECURSE. See
* http://forth.sourceforge.net/standard/fst83/FORTH-83.PRN
*
* \ ['] [CHAR] .S CELLS CHAR COMPILE, INVERT KEY? NIP POSTPONE SOURCE TUCK U>
* UNLOOP and WITHIN have been borrowed from the ANSI draft 6 specification.
* See http://www.forth.org/svfig/Win32Forth/DPANS94.txt
*
* RESTRICT is non-standard. It comes from GNU Forth (VolksForth). The " OK"
* non-prompt string also does, by the way. Thanks to Anton Ertl for his terse
* yet valuable input.
*
* .' (dot-tick) is non-standard. It comes from SwiftForth. It will be supported
* if symbolic stack dump has not been disabled (see SSDFEAT in constants.asm).
*
* MONITOR and ICHECK are also non-standard. They are intended to maintain and
* verify the integrity of a checksum of the code section of RAM resident
* words, with the exception of VARIABLEs and CREATEd words. CREATEd words,
* if subject to the MONITOR treatment will also carry a code section
* checksum, although this is not the default behaviour. The checksum will
* consist in an extra byte added to every words' header. MONITOR and ICHECK
* will only be available if the reliability feature has not been disabled
* (see RELFEAT in constants.asm).
*
* Forth source code portability note:
* Because Z79Forth does not use the return stack to store return addresses,
* it makes it quite easy to write code that is not readily portable to
* classic threaded interpretive implementations. The resulting code will be
* simpler but portability will be limited.
*
* Miscellaneous notes: RA stands for return address; EP for entry point.
* CF is a shortcut for CompactFlash. TOS means top of the data/normal stack.
* Occasionally, CFA is used for code field address which, in this
* implementation, is a synonym for compilation address. XT means execution
* token--the ANSI term for a compilation address. MSC stands for most
* significant cell; LSC for least significant cell.
*
* The complete guide to the HD6309 extended instruction set can be found at:
* https://cyberabi.ipower.com/Downloads/The_6309_Book.pdf. For convenience,
* and with the author's explicit agreement, this document has been included
* in this distribution. This is recommended reading for anyone altering this
* code or trying to make some sense of it!!!
*
* This whole effort started out as a porting endeavour and ended up being a
* substantial rewrite, trying to take advantage of the features of the 6309
* as much as possible. The primary goals of this implementation are:
* reliability, performance and code compactness. A number of bugs in the
* original code have been fixed--some remain for sure!
*
* Obligatory literary reference:
* "The paper is very heavy going, and I should never have read it, had I not
* written it myself." J. E. Littlewood (1885-1977).

pragma	opt cd,operandsizewarning

	include	constants.asm

* Unchecked NPUSH. This is for situations in which there is
* absolutely no chance of overflow. For instance, in case
* we just popped 2 cells and push back one or two. This
* always is inlined,
UCNPUSH	MACRO	NOEXPAND
	pshu	x
	ENDM

* Make sure minimum data stack requirements are satisfied upon word entry.
* The requirement is expressed in cell size--2 bytes on the 6309.
* \1 specifies the required number of cells.
MINDREQ	MACRO	NOEXPAND
	ldd	#NSTBOT-(\1*2)	Cell count one expects to be stacked up
	jsr	CHKNDPT		No return if the condition is not met
	ENDM

* Reliability feature support: execution tokens.
* \1 has the opcode we want to emit.
* \2 has the execution token address, as if the header overhead always
* was 3 bytes.
RFXT	MACRO	NOEXPAND
	IFNE	RELFEAT
	\1	\2+1
	ELSE
	\1	\2
	ENDC
	ENDM

* Reliability feature support: variable word header contents.
RFCS	MACRO	NOEXPAND
	IFNE	RELFEAT
	fcb	ILLOPC
	ENDC
	ENDM

*******************************************************************************
* RAM definitions. We cannot initialize globals from the assembly defs.
* All we can do here is define addresses and size things up.

* RAMSTART is 0. We leave page 0 as a bug proof area.
* When compiled in debug mode, this area is filled with illegal
* instruction opcodes ($C7).

	org	VARSPC

* Global pointers.
RSP	rmb	2		The return stack pointer
TOKENSP	rmb	2		Token start pointer (STRS)
TOKENEP	rmb	2		Token end pointer (STRE)
LSTWAD	rmb	2		Last defined word header pointer--LAST
DICEND	rmb	2		Current end of the dictionary--HERE
BLSTWAD	rmb	2		Backup (was IX)
BDICEND	rmb	2		Backup (was IY)
RECADDR	rmb	2		Used by RECURSE
JSRLAST	rmb	2		Last compilation address of #JSROPC
VLPRVEP	rmb	2		Used in VLIST to compute word code length
VLPRVHD	rmb	2		Used in VLIST to compute word code length
MRUBUFA	rmb	2		Most recently used buffer address

* Global variables.
UBASE	rmb	2		Base for numbers input and output--BASE
USTATE	rmb	2		0 if interpreting, 1 if compiling--STATE
UTOIN	rmb	2		User variable for >IN
UBLK	rmb	2		User variable for BLK
USCR	rmb	2		User variable for SCR (output for LIST)
BSBFADR	rmb	2		Base buffer address for the input stream
	IFNE	DEBUG
CCREG	rmb	2		A DEBUG variable for predicates (see CMP2)
	ENDC
CMDLNSZ	rmb	1		Entered character count in GETS (INTERP)
RDEPTH	rmb	1		Return stack depth in cells
IRDPTH	rmb	1		Return stack depth when : was last invoked
RTSREMV	rmb	1		If > 1, omit the final RTS when compiling
DIVFCN	rmb	1		Flag used by /, MOD and /MOD
STSLFCN	rmb	1		Flag used by */, */MOD
CVTFCN	rmb	1		CVT: 0 => # semantics, 1 => #S semantics
ISNEGF	rmb	1		Number being scanned is negative
CVISSGN	rmb	1		Flag: should CVNSTR consider numbers as signed
CURTOKL	rmb	1		Current token length. Set by SWDIC
IMDFLG	rmb	1		Immediate flag
DEFFLG	rmb	1		Define flag
NBCTFB0	rmb	1		NZ if -->/CONTINUED invoked from the console

CFCARDP	rmb	1		NZ if CF card present
CFCMMIR	rmb	1		Last CF command issued
CFERRCD	rmb	1		and the corresponding error code

PADBUF	rmb	PADBSZ		PAD lives here. Used by <#, #, #S, #> and DUMP

* The normal (data) stack.
	align	2
NSTTOP	equ	*		U's value when the data stack is full
	rmb	NSTKSZ
NSTBOT	equ	*		U's value when the data stack is empty

* The return stack.
RSTTOP	equ	*
	rmb	RSTKSZ
RSTBOT	equ	*

CMDBUF	rmb	CMDBFSZ
HEXBUF	rmb	HEXBFSZ

TBUFF	rmb	TBUFSZ		Output for CVNSTR. Also used by DUMP

	align	16
BUF0	rmb	BLKSIZ+4
	align	16
BUF1	rmb	BLKSIZ+4

*******************************************************************************
* Writable dictionary section begins right here. We cannot have anything
* statically defined at this point. Therefore, FORTHIN moves the code for @
* to this location and sets up DICEND and LSTWAD accordingly. All the RAM
* beyond this point (after the reallocated @ implementation) is user defined
* material. Note that builtin words, though they cannot be forgotten (to the
* extent they are ROM resident--not to mention inter-word dependencies),
* might still be overridden by user definitions. VLIST will happily ignore
* that fact and list everything in the dictionary linked list order.

	align	16
WDICSPC	equ	*

*******************************************************************************
* ROM code begins.

	org	ROMSTRT
* Trap handler: division by zero or illegal opcode. See page 4-10 of "The 6309
* Book" for a description of interrupt stacks in native 6309 mode.
IODZHDL	bitmd	#$40		Illegal opcode?
	beq	@iodzh1
	ldx	#IOPERRM
	bra	@iodxh2
@iodzh1	bitmd	#$80		Division by zero?
	beq	@iodzh3		And you may ask yourself, well
*				How did I get here?
	ldx	#DV0ERRM
@iodxh2 jsr	PUTS
	ldd	12,s		Return code address (PC)
	ldx	#HEXBUF
	jsr	HDMP4	
	ldx	#HEXBUF
	jsr	PUTS
	jsr	PUTCR
	ldx	#ERRHD1
	stx	12,s		Resume execution in the error handler
	ldx	#IODZHDL
	stx	8,s		With Y set to IODZHDL
	lda	#3		And user ABORT error code
	sta	2,s		Passed back through B
@iodzh3	rti

SWI3HDL	equ	*
SWI2HDL	equ	*
FIRQHDL	equ	*
IRQHDL	equ	*
SWIHDL	equ	*
NMIHDL	sync			Go to low power mode idling forever

RSTHDL	orcc	#$50		Disable interrupts

* In theory, this means 15% extra performance.
* In practice, this means an extra 5 mA current comsumption.
* The additional performance is measurable so I am going for it.
	ldmd	#1		Establish 6309 native mode

	lda	#ACIRSET
	sta	ACIACTL		ACIA master reset
	lda	#ACIRTS1
	sta	ACIACTL		No IRQ on RDRF, RTS high, 8N1, 115200 bps

	ldx	#BOOTMSG	Identity statement

* Send the NUL terminated string pointed to by X to the ACIA.
* The RAM is not assumed to be working.
INIT	ldb	#ACITDRE
@init0	lda	,x+
	beq	RAMCHK		NUL marks the end of the string
@init1	bitb	ACIACTL		Wait for TDRE bit to be set
	beq	@init1
	sta	ACIADAT		Transmit data
	bra	@init0		Next char, if any

* RAM0 32 KB self test (destructive).
RAMCHK	ldx	#RAMSTRT
@ramch1	stx	,x
	cmpx	,x
	bne	RAMFAIL
	leax	2,x
	cmpx	#RAMSTRT+RAMSIZE
	bne	@ramch1
	bra	RAMOK

RAMFAIL	ldb	#128
@ramf1	ldy	#$8000
@ramf2	leay	-1,y
	bne	@ramf2
	decb
	bne	@ramf1
	ldx	#RAMFM
	bra	INIT		Here we go again

* RAM Initialization to all $C7 if DEBUG mode is enabled else zeroes.
RAMOK	ldx	#RAMSTRT
	leay	1,x
	ldw	#RAMSIZE-1
	IFNE	DEBUG
	lda	#ILLOPC
	sta	,x
	ELSE
	clr	,x
	ENDC
	tfm	x+,y+

* Initialize the system stack pointer and the direct page base address register.
	lds	#RAMSTRT+RAMSIZE
	IFNE	USEDP
	lda	#VARSPC/256
	tfr	a,dp
	SETDP	VARSPC/256
	ENDC
	ldx	#RAMOKM
	jsr	PUTS
	jsr	FORTHIN		Global variables initialization
	jsr	CFINIT		CompactFlash card initialization

* The interpreter main loop.
INTERP	clrd
	std	UBLK		Interpreting from the console. Set BLK to 0
	std	UTOIN
	ldx	#CMDBUF
	stx	BSBFADR
	ldb	#CMDBFSZ-1	NUL terminator is not included in the char count
	jsr	GETS		Acquire command from the console
* Additional setup in case the ANSI \ is used.
	stb	CMDLNSZ		GETS returns the entered character count via B
* Additional preparation work for block support.
	clr	NBCTFB0		Flag whether -->/CONTINUED is called from blk 0
	bsr	_INTERP
MINTLRA	bra	INTERP

* The interpreter itself.
_INTERP	jsr	SCNSTOK		Scan for the beginning of a word at address X
	beq	@more0		This is the end
	tst	USTATE+1	We do ignore the upper byte
	bne	COMP		We are compiling
	jsr	SWDIC		Updates TOKENEP, CURTOKL, IMDFLG/DEFFLG
	bne	@exec		Word found, execute it
	jsr	NUMCVT
NMCVIRA	equ	*
	jsr	NPUSH
	ldx	TOKENEP
MORE	tst	,x
	bne	_INTERP		Next token, please!
* End of input stream condition is recognized.
@more0	ldd	UBLK
	beq	@more1		We are back from the console
	tst	NBCTFB0		-->/CONTINUED invoked from the console?
	bne	@more1		Yes
	rts			No, we're done here
@more1	clr	NBCTFB0		The -->/CONTINUED exception only applies once
	ldx	#OKPRMPT	Provide OK feedback
	tst	USTATE+1	No OK feedback if we're compiling, just CRLF
	beq	@more2
	leax	3,x		Skip the ' OK' string when compiling
@more2	jsr	PUTS
	rts			Back to whoever invoked us
@exec	lda	DEFFLG
	beq	@introk		Compilation only flag is not set
	ldb	#6		Incorrect STATE
	jsr	ERRHDLR		No return
INTISRA	equ	*		For symbolic stack debugging purposes
@introk	ldd	TOKENEP
	subd	BSBFADR
	std	UTOIN		Derive >IN from TOKENEP
	ldx	#INTRPRA	The return address
	pshs	x
	tfr	y,pc		An indirect call to Y
INTRPRA	jsr	BKIN2PT		Derive input stream pointer from BLK, >IN
	bra	MORE

* The compiler.
* Upon entry TOKENSP has been set by a prior call to SCNSTOK.
COMP	jsr	SWDIC		Updates TOKENEP, CURTOKL, IMDFLG/DEFFLG
	beq	@cmpnum		Word @ TOKENSP is not in the dictionary
	tst	IMDFLG
	beq	@notimd
	ldd	TOKENEP		Derive >IN from TOKENEP
	subd	BSBFADR
	std	UTOIN
* Immediate word processing. We execute the word with an option to compile
* an execution routine as well. This is achieved by allowing the callee to
* set an 'action component' address that resides at 2,s in the context
* of the (immediate) word being executed. Upon return, if that address is
* found to be NZ, a JSR to that address will be compiled. This feature is
* used in a number of words. DOES> is one of them.
	clrd
	pshs	d		Default action component address is none
	ldx	#COMPLRA	Word is IMD. Invoke it. Maybe compile it too
	pshs	x		Return to COMPLRA
	tfr	y,pc		An indirect call to Y
COMPLRA	jsr	BKIN2PT		Derive input stream pointer from BLK, >IN
	stx	TOKENEP
	puls	d		Code address possibly set by IMD word
	tstd
	beq	INTRPRA		Word has no action component
	tfr	d,y
@notimd	tfr	y,x
	ldy	DICEND
	lda	#JSROPC		JSR extended
	bsr	CHKRTS		Check if the final RTS can be omitted
	jsr	VARCON2		Compile a JSR to the action component
	sty	DICEND
	bra	@cmpdon		Branch back to the interpreter
@cmpnum	jsr	NUMCVT
NMCVCRA	equ	*
	jsr	LITER
@cmpdon	ldx	TOKENEP
	bra	MORE

* Check whether the final RTS can be eliminated. It can only be if we have no
* forward references to HERE when COMPR (;) is invoked. This is a rather
* complicated matter but this implementation works on the basis that we can do
* so safely if at least 2 subroutine calls have been issued with a return
* stack whose depth is equal to IRDPTH, immediately prior to the invokation
* of COMPR (;).
CHKRTS	pshs	a
	sty	JSRLAST		JSRLAST points to the latest JSR code emission
	lda	RDEPTH
	cmpa	IRDPTH		Return stack depth when : was last invoked
	beq	@ckrts1
	clr	RTSREMV
@ckrts0	puls	a
	rts
@ckrts1	inc	RTSREMV
	bra	@ckrts0

* Store the HEX representation of the lower nibble of A to X+.
HEX1D	pshs	b
	ldb	#'0
	anda	#$0f
	cmpa	#10
	bcs	@hex1d1
	ldb	#'A-10
@hex1d1	addr	b,a
	sta	,x+
	puls	b
	rts

ADIV16	lsra
	lsra
	lsra
	lsra
	rts

* Hexdump D to 4 bytes starting at X.  Upon return, X will point 1 byte
* after the last character emitted. D is preserved.
HDMP4	pshs	d
	bsr	ADIV16
	bsr	HEX1D
	lda	,s
	bsr	HEX1D
	lda	1,s
	bsr	ADIV16
	bsr	HEX1D
	lda	1,s
	bsr	HEX1D
	puls	d
	clr	,x
	rts

HDMP2	pshs	d
	bsr	ADIV16
	bsr	HEX1D
	lda	,s
	bsr	HEX1D
	puls	d
	clr	,x
	rts

* Add string pointed to by Y starting at the address stored in X.
ADDS	pshs	a
@adds1	lda	,y+
	sta	,x+
	bne	@adds1
	puls	a
	rts

* Returns the length of the string pointed to by X (terminator excluded) in W.
SLEN	pshs	x
	clrw
@slen1	incw
	lda	,x+
	bne	@slen1
	decw
	puls	x
	rts

	include	console.asm

	IFNE	DEBUG
PAGE0IN pshs	x		Fill up page 0 with all C7 (an illegal opcode)
	ldx	#RAMSTRT
	lda	#ILLOPC
	sta	,x
	leay	1,x
	ldw	#255
	tfm	x+,y+
	puls	x
	rts
	ENDC

FORTHIN	RFXT	jsr,NCLR+7	XT for NCLR. Set up the normal stack
	RFXT	jsr,RCLR+7	XT for RCLR. Set up the return stack
	IFNE	DEBUG
	bsr	PAGE0IN
	ENDC
* Relocate '@' code to RAM and set it up as the last dictionary entry (RO).
	ldx	#THEEND		Source address for tfm
	ldd	#REALEND
	subr	x,d
	tfr	d,w		Byte count for tfm
	ldy	#WDICSPC	Destination address for tfm
	sty	LSTWAD
	tfm	x+,y+
	sty	DICEND
	IFNE	RELFEAT
	RFXT	jsr,MONITOR+10	XT for MONITOR (monitor @ in RAM)
	ENDC
	bsr	EMPTYB		Buffer related initializations.
	clrd
	std	USTATE		Initial mode is interpretation
	std	USCR		Clear SCR
	RFXT	jmp,DECIMAL+10	XT for DECIMAL. Default base is decimal

EMPTYB	ldx	#BUF0
	bsr	EMPT1B
	ldx	#BUF1
* Empty the buffer pointed to by X.
EMPT1B	stx	MRUBUFA		Update most recently used buffer address
	leay	BLKSIZ+1,x
	clrd
	std	,y		Clear terminator and flags fields
	rts

* Scan for the next non-space character pointed to by X.
* That character is returned through A. Flags are set accordingly.
SCNSTOK	lda	,x+
	beq	@scstk1
	cmpa	#SP
	beq	SCNSTOK
@scstk1	leax	-1,x		Keep X pointing at the beginning of the token
	stx	TOKENSP
	tsta			Have to test again because LEA affects Z
	rts

* Scan for the next white space character (or NUL) as an end of token marker.
* Upon entry:
* - X points to the input stream.
* Upon return:
* - X will point to the next space character or NUL.
* - CURTOKL will hold the current token length (returned in B).
* - TOKENEP will point to the end of the current token.
SCNETOK	clrb
@scetok	incb
	lda	,x+
	beq	@scetk1
	cmpa	#SP
	bne	@scetok
@scetk1	leax	-1,x		Keep X pointing at the end of the token
	stx	TOKENEP
	decb
	stb	CURTOKL
	rts

* Check whether BASE is in the supported range ([2..36]).
CKBASE	lda	UBASE+1		BASE
	cmpa	#2
	blo	@ckbser		Must be >= 2
	cmpa	#36
	bhi	@ckbser		And <= 36 (the ANSi maximum)
	rts
@ckbser	ldb	#15		Invalid BASE
	jsr	ERRHDLR		No return
CKBASRA	equ	*

* NUMCVT performs a signed string to number conversion. The input string is
* acquired from the input stream. Note that this routine always is called
* after a dictionary lookup (SWDIC), so TOKENSP and CURTOKL are guaranteed
* to have been set previously. There are two ways out this routine:
* - redirection to the error handler (Undefined word), or
* - a converted cell returned through X.
* This a re-implementation based on CONVERT but unlike CONVERT, which produces
* an unsigned double as its output, this produces a signed single cell.
NUMCVT	bsr	CKBASE		No return if BASE isn't in the [2..36] range
	ldx	TOKENSP
	lda	CURTOKL		Character count to go through
	pshs	a
	clrd			Initialize the result
	sta	ISNEGF		Assume it be be non-negative
* Check for optional minus sign.
	ldf	,x
	cmpf	#'-
	bne	@ncnxt
	inc	ISNEGF		Remember to negate the result before returning
	leax	1,x		Skip the negativity!
	dec	,s		Decrement token length
@ncnxt	muld	UBASE		D multipled by BASE to Q (D:W)
	ldb	,x+		Acquire next input char (ignore the product MSC)
	subb	#'0
	blo	@ncoor		Digit out of range
	cmpb	#10
	blo	@ncnolt		No letter in potential BASE
	IFEQ	CSSNTVE
	cmpb	#'A-'0
	blo	@ncoor		Greater than 9 but lower than A
	cmpb	#'a-'0
	blo	@ncisuc		Upper case already
	cmpb	#'z-'0
	bhi	@ncoor		Definitely out of range
	subb	#'a-'A		To upper case
	ENDC
@ncisuc	subb	#'A-':		A-Z to number
@ncnolt	cmpb	UBASE+1		B has a digit. Make sure it's less than BASE
	bhs	@ncoor
* Digit in B has been validated wrt. BASE.
	clra
	addr	w,d
	dec	,s		Are we done yet?
	bne	@ncnxt		No. Iterate over to the next digit
	tst	ISNEGF		Are we dealing with a negative number?
	beq	@ncdone		No.
	negd			Acknowledge the negativity
@ncdone	leas	1,s		Drop token length from the system stack
	tfr	d,x		Return the result in X
	rts
@ncoor	leas	1,s		Drop token length from the system stack
	ldx	TOKENSP		Current digit is out of range
	ldb	#2		Undefined (X points to the offending word)
	jsr	ERRHDLR		No return
NUMCVRA	equ	*		For symbolic stack dump purposes

* Check for minimal data depth. On input D has the lowest possible stack
* address that satisfies the needs of the caller. This routine is meant
* to support "transactional" behaviour, which is intended to improve
* debugging support.
CHKNDPT	cmpr	d,u
	bhi	@stkudf
	rts
@stkudf	ldb	#1		Data stack underflow
	jsr	ERRHDLR		No return
CKDPTRA	equ	*

* Convert number stored in X to a string (depending on UBASE value).
* Output is stored in the global TBUFF buffer. X is preserved.
CVNSTR	bsr	CKBASE
	clr	ISNEGF
	tfr	x,d
	tst	CVISSGN		Are we to perform a signed conversion?
	beq	@cvnst1
	tstd
	bpl	@cvnst1
	inc	ISNEGF		Number being converted is negative
	negd
@cvnst1 tfr	d,w
	clrd
	ldy	#TBUFF+19	Sign + 16 digits + terminator + 1
	clr	,-y		End of string marker
@cvnst2	divq	UBASE		D returns the modulo and W the quotient
	lda	#'0
	cmpb	#10
	bcs	@cvnst3
* Base has letters among its valid numbers.
	lda	#'A-10
@cvnst3	addr	a,b
	stb	,-y
	clrd
	tstw
	bne	@cvnst2
	ldb	#SP
	tst	ISNEGF
	beq	@cvnst4
	ldb	#'-
@cvnst4	stb	,-y
* We need to insert leading spaces up to the beginning of the output buffer.
	ldb	#SP
@cvnst5	cmpy	#TBUFF
	bne	@cvnst6
	rts
@cvnst6	stb	,-y
	bra	@cvnst5

* Search word beginning at address TOKENSP in the dictionary.
* Upon return Z will be set if the word was not found.
* Otherwise, Y will point to the code section of the word.
* CSSNTVE (defined in constants.asm) defines whether the
* dictionary match is case sensitive or not.
* Important note: if the word is found TOKENEP will be copied to TOKENSP.
SWDIC	ldx	TOKENSP
	jsr	SCNETOK		B has CURTOKL
	ldx	TOKENSP
	ldy	LSTWAD		Latest word header address to Y
@swrdc0	bne	@swrdc1
	tfr	0,y		Word not found. Z is set
	rts
@swrdc1	lda	,y		Word attribute to A
	anda	#WRLNMSK	Extract word length
	pshs	y
	cmpr	b,a		Word length match?
	bne	@swrdc3		No, point to next dictionary entry
	leay	1,y
@swrdc2	lda	,y+
	IFNE	CSSNTVE
	cmpa	,x+		Case sensitive dictionary
	ELSE
	tfr	b,e		Case insensitive search (preferred)
	ldb	,x+
	cmpb	#'a
	bcs	@nochg
	cmpb	#'z+1
	bcc	@nochg
	subb	#'a-'A
@nochg	cmpr	b,a
	tfr	e,b
	ENDC
	bne	@swrdc3
	decb
	bne	@swrdc2
* Word match!
	puls	x
	ldb	,x		Word attribute byte to B
	clra
	lslb			Bit 7 to CFLAG
	rola			CFLAG to A
	sta	IMDFLG		Set IMMEDIATE flag
	clra
	lslb			Bit 6 to CFLAG
	rola			CFLAG to A
	sta	DEFFLG		Set DEFINE flag
	ldx	TOKENEP
	stx	TOKENSP
	IFNE	RELFEAT
	leay	3,y		Skip back pointer and checksum. Return XT
	ELSE
	leay	2,y		Skip back pointer. Return XT
	ENDC
	rts			Z is clear due to the latest leay invocation
@swrdc3	puls	y		Point to previous word in the dictionary
	clra
	ldb	,y+
	andb	#WRLNMSK
	leay	d,y
	ldx	TOKENSP
	ldb	CURTOKL
	ldy	,y
	bra	@swrdc0

* Create new dictionary entry. The word name being created is acquired from
* the input stream. Warning, this requires a writable dictionary!
* The new end of the dictionary is returned in Y. W is preserved.
LOCWRT	pshsw
	ldx	DICEND
	IFNE	DEBUG
	cmpx	#ROMSTRT
	bcs	@locwr0
	ldb	#10		Assertion failure (trying to write to ROM!)
	jsr	ERRHDLR		No return
LWAFRA	equ	*
	ENDC
@locwr0	stx	BDICEND		Back pointer up
	ldx	LSTWAD
	stx	BLSTWAD		Back pointer up
	jsr	BKIN2PT		Derive input stream pointer from BLK, >IN
	tst	,x
	bne	@locwr2
@locwr1	ldb	#5		Missing word name
	jsr	ERRHDLR		No return
LWMNRA	equ	*		LOCWRT missing word name return address
@locwr2	jsr	SCNSTOK
	beq	@locwr1		End of line reached
	jsr	SCNETOK		X has TOKENEP, B has CURTOKL
	ldy	TOKENSP
	subr	y,x
	pshs	x		Word length to the system stack
	ldx	DICEND
	lda	1,s		Word length LSB in the system stack
	cmpa	#1+WRLNMSK	Max word length is 31, 79-STANDARD compliant
	blo	@lcwr21
	ldb	#16		Word name is too long
	jsr	ERRHDLR
WTOOLNG	equ	*
@lcwr21	sta	,x+		Word length to dictionary
	ldw	,s++		16-bit word length to W
	exg	x,y		Y points to the dictionary, X has TOKENSP
	IFNE	CSSNTVE
	tfm	x+,y+		Word name to dictionary, as is
	ELSE
	tfr	f,b		Force dictionary entry to upper case
@locwr3	lda	,x+
	cmpa	#'a
	bcs	@locwr4
	cmpa	#'z+1
	bcc	@locwr4
	suba	#'a-'A		To upper case
@locwr4	sta	,y+
	decb
	bne	@locwr3
	ENDC
	tfr	x,d
	subd	BSBFADR
	std	UTOIN		Skip word name in the input stream
	ldx	LSTWAD
	stx	,y++		Back pointer to dictionary
	IFNE	RELFEAT
	clr	,y+		Initialize the checksum header field
	ENDC
	sty	DICEND
	sty	RECADDR		Should we resort to recursion later on
	pulsw
	rts

* Compile 'ldx	#X; jsr NPUSH'.
* DICEND is updated and returned in Y.
LITER	ldy	DICEND
	lda	#LDXOPC		LDX immediate
	bsr	VARCON2
	lda	#JSROPC		JSR extended
	jsr	CHKRTS		Check if the final RTS can be omitted
	bsr	VARCON1
	sty	DICEND
	rts

* Compile 'jmp	NPUSH'.
* Y points to the end of the dictionary on entry and on exit.
VARCON	lda	#JMPOPC		JMP extended
VARCON1	ldx	#NPUSH
VARCON2	sta	,y+
	stx	,y++
	rts

* Used by U<, U>, <, >.
CMP2	MINDREQ	2		At least 2 cells must be stacked up
CMP2RA	ldy	,u
	ldx	2,u
	cmpr	y,x
	tfr	cc,b
	IFNE	DEBUG
	clra
	std	CCREG
	ENDC
	tfr	0,x
	leau	4,u
	tfr	b,cc
	rts

* Used by CMOVE, <CMOVE, CMOVE>, MOVE.
ACQMOVP	MINDREQ	3		At least 3 cells must be stacked up
ACQVMRA	ldw	,u		Byte count
	ldy	2,u		Destination address
	ldx	4,u		Source address
	leau	6,u		Drop 3 cells from the user stack
	rts

* FDCTSYM enveadours to match the address stored in Y to a dictionary entry.
* Upon a successful flexible match, it will output a string in the form of
* <wordname>+<offset> to the string pointed to by X. <wordname> refers to the
* code entry point of the matched word. <offset> will be expressed in hex.
* If a match is found, Z will be clear, otherwise it will be set.
* A candidate for a word code address must meet the following criteria:
* ((Y U>= #EBUFS) AND (Y U< #THEEND)) OR ((Y U>= #WDICSPC) AND (Y U< DICEND)).
* Upon return:
* - the string pointed to by X will updated with its symbolic match
*   (NUL terminated), if there is one. X itself might have been altered.
* - Y should be preserved.
* EBUFS (EMPTY-BUFFERS) header address is the last word in the dictionary.
	IFNE	SSDFEAT
FDCTSYM	pshs	y,x	
	tfr	y,x		Potential execution token to X
	cmpx	#EBUFS
	blo	@fdstr2	
	cmpx	#THEEND
	blo	@fdsmtc
@fdstr2	cmpx	#WDICSPC	Term 2 of the predicate for a valid word addr
	blo	@fdsnom
	cmpx	DICEND
	blo	@fdsmtc
@fdsnom	clra			No match (Z is set)
	puls	x,y
	rts
@fdsmtc	ldx	LSTWAD		Potential match. Scan upward from LAST
* X points to the latest word header, Y has an execution token.
	ldw	DICEND		W points to the end of the code section
@fdslop	pshs	x		Pointer to the current word header
	ldb	,x+
	andb	#WRLNMSK
	abx			Skip word name string
	ldd	,x++		Backlink to D
	IFNE	RELFEAT
	leax	1,x		Skip the checksum
	ENDC
	cmpr	x,y
	blo	@fdsnwd
	cmpr	w,y
	bhi	@fdsnwd		An equal address is allowed here, in case
* jsr ERRHDLR is the last instruction for the word under consideration.
* This happens to be the case for FORGET and LPAR.
* Y matches the code range for the current word.
	puls	x
	ldb	,x+
	andb	#WRLNMSK
	tfr	b,f
	clre			W has the matched word length
	ldy	,s		Y points to the target buffer
	lda	#''
	sta	,y+
	tfm	x+,y+
	sta	,y+
* Offset processing.
	lda	#'+
	sta	,y+
	IFNE	RELFEAT
	leax	3,x		Skip backlink and checksum
	ELSE
	leax	2,x		Skip backlink
	ENDC
	ldd	2,s		Execution token to D
	subr	x,d		Offset between XT and word entry point to D
	tfr	y,x
	jsr	HDMP4		Dump hex incarnation of the offset to X
	puls	x,y
	andcc	#^ZFLAG		Clear ZFLAG
	rts
* Point to the next word.
@fdsnwd	pulsw			Retrieve current word header address
	tstd
	beq	@fdsnom		Just met the last dictionary entry
	tfr	d,x		Point to previous word header
	cmpw	#WDICSPC	Are we transitioning from RAM to ROM?
	bne	@fdslop		No singularity
	ldw	#THEEND		Won't be able to diagnose the ROM based @...
	bra	@fdslop

* Find the best possible symbolic approximation of Y and store it to the
* buffer pointed to by X.
* On entry:
* - Y has a code address which is to be matched with a symbolic name + offset.
* - X points to a buffer where the resolution is to be stored.
* On return:
* - Y is to be preserved.
* - the output buffer will be updated with the best symbolic match (NUL term'd).
FINDSYM	pshs	y,x
	bsr	FDCTSYM		Search the dictionary first (flexible match)
	bne	@dctmfn		Dictionary match found
	ldx	#NDCTWKS	Search kernel symbols for an exact match
@fsmlop	ldd	,x++
	beq	@fsmfnd		End of well known symbols list?
	cmpr	y,d
	beq	@fsmfnd
* No well known symbol match. Skip string.
@fsmskp	lda	,x+
	bne	@fsmskp
	bra	@fsmlop
@fsmfnd	ldy	,s
@fsmfn2	lda	,x+
	sta	,y+
	bne	@fsmfn2
@dctmfn	leas	2,s
	puls	y
	rts

* Non-dictionary well known symbols.
NDCTWKS	fdb	IODZHDL		Illegal opcode/Division by zero trap handler
	fcn	'IODZHDL'
	fdb	DPOPRA		Data stack underflow
	fcn	'DPOPRA'
	fdb	DPSHRA		Data stack overflow
	fcn	'DPSHRA'
	fdb	RPOPRA		Return stack underflow
	fcn	'RPOPRA'
	fdb	RPSHRA		Return stack overflow
	fcn	'RPSHRA'
	fdb	ERRHDLR		Error handler
	fcn	'ERRHDLR'
	fdb	CKBASRA		Illegal BASE value
	fcn	'CKBASRA'
	fdb	CKDPTRA		Not enough parameters supplied (transac. behav.)
	fcn	'CKDPTRA'
	fdb	CMP2RA		Missing operand in any of U<, U>, <, >
	fcn	'CMP2RA'
	fdb	ACQVMRA		Three operands missing in any of CMOVE,
*				<CMOVE, CMOVE>, MOVE
	fcn	'ACQVMRA'
	fdb	MINTLRA		Main interpreter loop return address
	fcn	'MINTLRA'
	fdb	NUMCVRA		? while converting a string to a number
	fcn	'NUMCVRA'
	fdb	NMCVIRA		Numeric conversion error while interpreting
	fcn	'NMCVIRA'
	fdb	NMCVCRA		Numeric conversion error while compiling
	fcn	'NMCVCRA'
	fdb	INTRPRA		Interpreter RA (after the execution of a word)
	fcn	'INTRPRA'
	fdb	_INTERP		Interpreter actual active subroutine
	fcn	'_INTERP'
	fdb	COMPLRA		Compiler RA (after the execution of an IMD word)
	fcn	'COMPLRA'
	fdb	LWMNRA		Missing word name in LOCWRT
	fcn	'LWMNRA'
	fdb	WTOOLNG		Word length is too long
	fcn	'WTOOLNG'
	fdb	CFR1SRA		CF read one sector failed
	fcn	'CFR1SRA'
	fdb	NPUSH		Not an error RA but useful to have as a symbol
	fcn	'NPUSH'
	fdb	RPUSH		Not an error RA but useful to have as a symbol
	fcn	'RPUSH'
	fdb	NPOP		Not an error RA but useful to have as a symbol
	fcn	'NPOP'
	fdb	RPOP		Not an error RA but useful to have as a symbol
	fcn	'RPOP'
	fdb	PUTS		Not an error RA but useful to have as a symbol
	fcn	'PUTS'
	fdb	BK2BBRA
	fcn	'BK2BBRA'	Block BLK @ not mapped in INTERPRET
	IFNE	DEBUG
	fdb	LWAFRA		Assertion failure in LOCWRT
	fcn	'LOCWRTAF'
	ENDC
	fdb	0		End of list
	fcn	'???'		Admit we have no clue!
	ENDC			SSDFEAT

* Handle error condition. Error code is in B.
* If B is 2 (undefined) X points to a string of length CURTOKL that has the
* offending word.
ERRHDLR ldy	,s		Invoking return address
* In case of a trap return, we enter here with Y set to #IODZHDL
ERRHD1	cmpb	#2		Undefined symbol?
	bne	@perrm		No
	lda	#''
	jsr	PUTCH
@prtsym	lda	,x+		Display undefined symbol name
	jsr	PUTCH
	dec	CURTOKL
	beq	@endquo
	bra	@prtsym
@endquo	lda	#''
	jsr	PUTCH
	lda	#SP
	jsr	PUTCH
@perrm	ldx	#ERRMTBL	Regular error handling
@nxterr	tstb
	bne	@skerrm
	jsr	PUTS
	jsr	PUTCR
@dmptos	tfr	y,d		Dump top of the system stack contents
	ldx	#HEXBUF
	jsr	HDMP4
	lda	#SP
	sta	,x+
* Symbolic stack dumps are configurable. In situations where the dictionary is
* trashed, they may not be considered desirable. See SSDFEAT in constants.asm.
	IFNE	SSDFEAT
	jsr	FINDSYM
	ENDC
	ldx	#HEXBUF
	jsr	PUTS
	jsr	PUTCR
	cmpy	#IODZHDL
	beq	@wastrp		We're just back from the trap handler
	leas	2,s		Point to the next item on the stack
@wastrp	cmps	#RAMSTRT+RAMSIZE
	beq	@errdon		We're done here
	ldy	,s
	bra	@dmptos
@skerrm	lda	,x+		Scan for the next error message
	bne	@skerrm
	decb
	bra	@nxterr
@errdon	lds	#RAMSTRT+RAMSIZE
	lda	USTATE+1	We do ignore the upper byte
	beq	@erdon2		No pointers to restore if we were interpreting
* Compiling: clear STATE, RSP and restore LSTWAD, DICEND.
	clr	USTATE+1	Switch back to interpretation mode
	RFXT	jsr,RCLR+7	XT for RCLR
	ldx	BDICEND		Restore essential pointers from backups
	stx	DICEND		Restore HERE
	ldx	BLSTWAD
	stx	LSTWAD		Restore LAST
@erdon2	jmp	INTERP

* Push X to the data stack (boundary is checked).
NPUSH	cmpu	#NSTTOP
	bls	@npush1		Anything <= than #NSTTOP indicates overflow
	pshu	x		Aka UCNPUSH
	rts
@npush1	clrb			Data stack overflow
	jsr	ERRHDLR		No return
DPSHRA	equ	*
	nop

* Pull X from the data stack (boundary is checked).
* D, W and Y are preserved.
NPOP	cmpu	#NSTBOT
	bhs	@npop1		Anything >= than #NSTBOT indicates underflow
	pulu	x
	rts
@npop1	ldb	#1		Data stack underflow
	jsr	ERRHDLR		No return
DPOPRA	equ	*
	nop

* Push X to the return stack (boundary is checked).
RPUSH	lda	RDEPTH		RDEPTH is expressed in cells
	cmpa	#RSTKSZ/2	But RSTKZ is expressed in bytes
	beq	@rpush1
	inca
	sta	RDEPTH
	clr	RTSREMV		Do not get rid of the final RTS
	tfr	y,v
	ldy	RSP
	stx	,--y
	sty	RSP
	tfr	v,y
	rts
@rpush1	ldb	#7		Return stack overflow
	jsr	ERRHDLR		No return
RPSHRA	equ	*
	nop			Meant to insulate RPUSH errors from RPOP EP

* Pull X from the return stack (boundary is checked).
RPOP	lda	RDEPTH		RDEPTH is expressed in cells
	beq	@rpop1
	deca
	sta	RDEPTH
	tfr	y,v
	ldy	RSP
	ldx	,y++
	sty	RSP
	tfr	v,y
	rts
@rpop1	ldb	#8		Return stack underflow
	jsr	ERRHDLR		No return
RPOPRA	equ	*

* Derive a workable base buffer address from BLK and return it through X.
* Update BSBFADR accordingly. The buffer in question is assumed to have been
* mapped previously (through a call to BLOCK). An assertion failure will be
* raised if that is not the case. W is preserved, Y is untouched.
BLK2BB	pshsw
	ldw	UBLK
	bne	@bufadr
	ldx	#CMDBUF		Base buffer address for the serial line
	bra	@rslvd
@bufadr	ldx	#BUF0		Buffer 0 base address
	ldb	#2		Number of resident buffers
@chkflg	lda	BOFLAGS,x	Buffer flags to A
	anda	#BINUSE|BMAPPD
	cmpa	#BINUSE|BMAPPD	Is this buffer in use and mapped in?
	bne	@nxtbuf
	cmpw	BOBLKNO,x	Is this the block we are looking for?
	beq	@rslvd		Yes
@nxtbuf	leax	BFDISP,x	Point to the next buffer
	decb
	bne	@chkflg
@asfail	ldb	#10		Assertion failure
	jsr	ERRHDLR		No return
BK2BBRA	equ	*		For symbolic system stack debugging purposes
@rslvd	stx	BSBFADR		Base buffer address is resolved through X
	pulsw
	rts

* Derive the current input stream pointer from BLK and >IN.
* Y and W are untouched. The resulting address is returned in X.
* D is altered.	W is preserved, Y is untouched.
BKIN2PT	bsr	BLK2BB		Retrieve buffer address based on BLK's value
	ldd	UTOIN
	leax	d,x		Add the current offset. Return the result via X
	rts

	include	storage.asm

******************************************************************************
* Dictionary begins. In the code below ANSI refers to ANSI-X3.215-1994
* Draft 6 proposal (i.e. the free spec).

EBUFS	fcb	13		79-STANDARD (REQ145)
	fcc	'EMPTY-BUFFERS'	( -- )
	fdb	0		Last dictionary entry
	RFCS
	jmp	EMPTYB

SAVBUF	fcb	12		79-STANDARD (REQ221)
	fcc	'SAVE-BUFFERS'	( -- )
	fdb	EBUFS
	RFCS
	ldx	#BUF0
	bsr	WBIFDRT
	ldx	#BUF1
* Write buffer back to mass storage if marked as dirty.
* The dirty bit is cleared but the buffer contents itself is not.
* The buffer will continue to be marked as "in use."
* On input X has has the base buffer address. Both D and X
* are preserved. Y is not.
WBIFDRT	tfr	d,y
	pshs	x		Base buffer address (arg1 to CF1BKWR)
	lda	#BINUSE|BDIRTY
	anda	BOFLAGS,x
	cmpa	#BINUSE|BDIRTY
	bne	@alldon		Block not in use or in use but not dirty
	ldx	BOBLKNO,x
	pshs	x		Block number (arg0 to CF1BKWR)
	bsr	CF1BKWR		Write data buffer to CF
	leas	2,s		Drop one cell from the system stack (blknum)
	ldx	,s		Retrieve base buffer address
	lda	BOFLAGS,x
	anda	#^BDIRTY	Clear the dirty bit
	sta	BOFLAGS,x	and update the 'flags' field
@alldon	puls	x		Restore X
	tfr	y,d		and D
	rts

FLUSH	fcb	5		79-STANDARD (REF)
	fcc	'FLUSH'		( -- ) An alias for SAVE-BUFFERS
	fdb	SAVBUF
	RFCS
	RFXT	bra,SAVBUF+15	XT for SAVE-BUFFERS

UPDATE	fcb	6		79-STANDARD (REQ229)
	fcc	'UPDATE'	( -- )
	fdb	FLUSH
	RFCS
	ldx	MRUBUFA		Most recently used buffer base address
	lda	BOFLAGS,x	Buffer 'flags' field to A
	bita	#BINUSE		Is that buffer in use?
	beq	@upddon		No, we're done here
	ora	#BDIRTY
	sta	BOFLAGS,x	Set the dirty bit
@upddon	rts

BUFFER	fcb	6		79-STANDARD (REQ130)
	fcc	'BUFFER'	( ublkno -- addr )
	fdb	UPDATE
	RFCS
	jsr	NPOP
	tfr	x,y		ublkno to Y
* Block lookup.
	ldx	#BUF0		Base address of the first resident buffer
	ldb	#2		Number of resident buffers
@blkup	lda	BOFLAGS,x	Buffer flags to A
	bita	#BINUSE		Buffer in use?
	beq	@nxtbuf		No
	cmpy	BOBLKNO,x	Buffer is in use. Block number match?
	bne	@nxtbuf		No
@retba	UCNPUSH			Block number match. Return the base address
	rts
@nxtbuf	leax	BFDISP,x	Point to the next buffer
	decb
	bne	@blkup
* The block number in Y is not currently in use. Assign a buffer to it.
	ldx	#BUF0
	cmpx	MRUBUFA		Most recently used buffer address
	bne	@bselct
	ldx	#BUF1
@bselct	stx	MRUBUFA
	pshs	y		Backup the target block number
	jsr	WBIFDRT		Write back if dirty. X and D are preserved
	lda	#BINUSE
	sta	BOFLAGS,x	Update the buffers 'flags' field
	puls	y		Restore the target block number
	sty	BOBLKNO,x	and update the 'blknum' field as well
	bra	@retba

BLOCK	fcb	5		79-STANDARD (REQ191)
	fcc	'BLOCK'		( ublkno -- addr )
	fdb	BUFFER
	RFCS
	RFXT	bsr,BUFFER+9	XT for BUFFER
* Upon return Y has has the block number.
	jsr	NPOP		Buffer base address to X
	pshs	x		Push base buffer address as Arg1 to CF1BKRD
	lda	BOFLAGS,x	Retrieve buffer 'flags' field
	IFNE	DEBUG
	bita	#BINUSE
	bne	@blkctd
	lda	#10		Assertion failed
	jsr	ERRHDLR		No return
	ENDC
@blkctd	anda	#BMAPPD		Has the block been read yet?
	bne	@bkmapd		yes
	pshs	y		no. Push block number as arg0 to CF1BKRD
* Map in the block from the CF device. System stack structure is as follows:
* ,s has the target block number.
* 2,s has the buffer base address.
	jsr	CF1BKRD
	leas	2,s		Drop one cell from the system stack
* Update the buffer flags field.
	ldx	,s		Base buffer address
	lda	BOFLAGS,x	Buffer 'flags' field to A
	ora	#BMAPPD
	sta	BOFLAGS,x	Update buffer 'flags' field
@bkmapd	puls	x		Buffer base address to X
	UCNPUSH
	rts

BLK	fcb	3
	fcc	'BLK'
	fdb	BLOCK
	RFCS
	ldx	#UBLK
	jmp	NPUSH

TOIN	fcb	3
	fcc	'>IN'
	fdb	BLK
	RFCS
	ldx	#UTOIN
	jmp	NPUSH

SCR	fcb	3
	fcc	'SCR'
	fdb	TOIN
	RFCS
	ldx	#USCR
	jmp	NPUSH

* Functionally: : LINE 6 SHIFT SCR @ BLOCK + ;
LINE	fcb	4		79-STANDARD (REF)
	fcc	'LINE'
	fdb	SCR
	RFCS
	ldx	#6
	jsr	NPUSH
	RFXT	jsr,SHIFT+8	XT for SHIFT
	ldx	USCR
	jsr	NPUSH
	RFXT	bsr,BLOCK+8	XT for BLOCK
	RFXT	jmp,PLUS+4	XT for +

* Functionally:
* : INDEX 1+ SWAP DO
*     CR   I SCR !
*     0 LINE 64 TYPE
*   LOOP ;
INDEX	fcb	5		79-STANDARD (REF)
	fcc	'INDEX'		( n1 n2 -- )
	fdb	LINE
	RFCS
	jsr	NPOP		N2 to X
	leax	1,x		1+
	tfr	x,y		Y has the limit (not reached)
	jsr	NPOP		X has the index (N1)
@indlop	jsr	PUTCR		CR
	stx	USCR		I SCR !
	pshs	y,x
	tfr	0,x
	jsr	NPUSH
	RFXT	bsr,LINE+7	XT for LINE
	ldx	#64
	jsr	NPUSH
	RFXT	jsr,TYPE+7	XT for TYPE
	puls	x,y		Restore loop parameters
	leax	1,x
	cmpr	x,y
	bne	@indlop
	rts

LIST	fcb	4		79-STANDARD (REQ109)
	fcc	'LIST'		( ublkno -- )
	fdb	INDEX
	RFCS
	tst	CFCARDP
	bne	@lstpro
	ldb	#17		IO error
	jsr	ERRHDLR		No return
@lstpro	RFXT	jsr,DUP+6	XT for DUP
	RFXT	jsr,BLOCK+8	XT for BLOCK
* TOS now has the base buffer address.
	jsr	NPOP
	tfr	x,y		Base buffer address to Y
	jsr	NPOP		ublkno to X
	stx	USCR		Update SCR's value
	ldb	#16		16 lines to go
@lstlop	pshs	b
	jsr	PUTCR
	tfr	y,x
	jsr	NPUSH		Start address for TYPE
	ldx	#64
	jsr	NPUSH		Byte count for TYPE
	addr	x,y
	RFXT	jsr,TYPE+7	XT for TYPE
	puls	b
	decb
	bne	@lstlop
	rts

NCLR	fcb	4		Non-standard
	fcc	'NCLR'		Clear the data (normal) stack
	fdb	LIST
	RFCS
	ldu	#NSTBOT
	rts

RCLR	fcb	4		Non-standard
	fcc	'RCLR'		Clear the return stack
	fdb	NCLR
	RFCS
	ldx	#RSTBOT
	stx	RSP
	clr	RDEPTH
	rts

DEPTH	fcb	5		79-STANDARD (REQ238)
	fcc	'DEPTH'
	fdb	RCLR
	RFCS
	ldd	#NSTBOT		Bottom data stack address
	subr	u,d		D has the current value of the data stack ptr
	lsrd			divided by 2
	tfr	d,x
	jmp	NPUSH

CREATE	fcb	6		79-STANDARD (REQ239)
	fcc	'CREATE'
	fdb	DEPTH
	RFCS
CREAT0	jsr	LOCWRT		Code entry point returned to Y
	lda	#LDXOPC		LDX immediate
	sta	,y+
	tfr	y,x
	leax	8,x
	stx	,y++
	lda	#JSROPC		JSR extended
	jsr	VARCON1		Compile JSR NPUSH
	lda	#RTSOPC		RTS inherent
	sta	,y+
	leay	2,y		Reserve room for a possible DOES> clause
CREAT1	sty	DICEND
	ldx	BDICEND		This is set by LOCWRT (was IY)
	stx	LSTWAD
	rts

DOES	fcb	$C5		79-STANDARD (REQ168)
	fcc	'DOES>'
	fdb	CREATE
	RFCS
	ldx	#DOESEX		JSR #DOESEX is compiled (no actual return)
	stx	2,s		Set as action component
	rts

DOESEX	ldx	LSTWAD		Header of the last dictionary entry
	ldb	,x+
	andb	#WRLNMSK	Extract word length
	abx
	IFNE	RELFEAT
	leax	9,x		2 bytes/backlink, 1/cksum, 6 bytes code offset
	ELSE
	leax	8,x		2 bytes/backlink, 6 bytes code offset
	ENDC
	lda	,x
	cmpa	#RTSOPC		RTS inherent
	beq	@dosex1
	ldb	#14		No matching CREATE
	jsr	ERRHDLR		No return
@dosex1	lda	#JMPOPC		JMP extended
	sta	,x+		Overwrite RTS opcode
	puls	y
	sty	,x
	rts

LITERAL	fcb	$87		79-STANDARD (REQ215)
	fcc	'LITERAL'
	fdb	DOES
	RFCS
	jsr	NPOP
	lda	USTATE+1
	beq	@nojoy		The standard defines no semantics in interp mode
@comp	ldy	DICEND
	lda	#LDXOPC		LDX immediate
	jsr	VARCON2		Compile LDX #X
	sty	DICEND		Update HERE
	ldx	#NPUSH
	stx	2,s		Set NPUSH as action component
@nojoy	rts

* Functionally: : CONSTANT CREATE , DOES> @ ;
* The following code produces more compact code.
CONS	fcb	8		79-STANDARD (REQ185)
	fcc	'CONSTANT'
	fdb	LITERAL
	RFCS
	jsr	NPOP
	tfr	x,w
	jsr	LOCWRT		Create dictionary entry
	tfr	w,x
	lda	#LDXOPC		ldx immediate
	jsr	VARCON2		Compile LDX #CSTVAL
	jsr	VARCON		Compile JMP NPUSH
	IFNE	RELFEAT
	bsr	CREAT1
	RFXT	bra,MONITOR+10	XT for MONITOR
	ELSE
	bra	CREAT1
	ENDC

* Functionally: : VARIABLE CREATE 2 ALLOT ;
* However we can save three bytes per instance with the following code.
VARI	fcb	8		79-STANDARD (REQ227)
	fcc	'VARIABLE'
	fdb	CONS
	RFCS
	jsr	LOCWRT
	lda	#LDXOPC		ldx immediate
	sta	,y+
	tfr	y,w		Preserve Y (HERE)
	leay	5,y		Relative variable address
	tfr	y,x
	tfr	w,y		Restore Y (HERE)
	stx	,y++		Address field for LDX #VARADDR
	jsr	VARCON		Compile JMP NPUSH
	leay	2,y		2 ALLOT
	jmp	CREAT1

IMMED	fcb	9		79-STANDARD (REQ103)
	fcc	'IMMEDIATE'
	fdb	VARI
	RFCS
	ldb	#IMDFLM
IMMED1	ldx	LSTWAD
	lda	,x
	orr	b,a
	sta	,x
	rts

RSTRCT	fcb	8		Non-standard (GNU Forth)
	fcc	'RESTRICT'	Make word available in compilation mode only
	fdb	IMMED
	RFCS
	ldb	#DEFFLM
	bra	IMMED1

	IFNE	RELFEAT
* This non-standard word enables checkum monitoring by ICHECK for the
* last defined word in the dictionary. : words are monitored by default
* and so are constants. CREATEd words require an explicit invokation of
* MONITOR if they are to checked for integrity.
MONITOR	fcb	7
	fcc	'MONITOR'	( -- )
	fdb	RSTRCT
	RFCS
	ldx	LSTWAD		Last word header address
	lda	,x
	ora	#MONFLM		Set the monitored flag in the attribute field
	sta	,x
	bsr	HDRSKIP		Skip the header (XT to X), clear A
	ldy	DICEND		The upper code section limit (excluded)
	bsr	HDRCSUM		Current word's definition's checksum to A
	sta	-1,x		Store the computed checksum into the header
	rts

* On entry, X has a word's header address. On return X has the compilation
* address for that word (XT). A is cleared so as to initialize the checksum.
HDRSKIP	lda	,x		Word's header attribute byte to A
	anda	#WRLNMSK	Extract the word's length
	adda	#4		1B attribute, 2B backlink, 1B checksum
	leax	a,x		X has the word's XT. -1,X has the orig checksum
	clra
	rts

* On entry X has a word's compilation address (XT). A is supposed to have been
* previously cleared. Y has the upper bound of the code section's address
* (excluded). On return, A will have the word's code section's checksum.
* X is preserved.
HDRCSUM	pshs	x
@hdrcs1	eora	,x+		Update the checksum
	cmpr	x,y		Upper bound reached?
	bne	@hdrcs1		No. Process next byte in word's code section
	puls	x
	rts

* This non-standard word walks through the dictionary linked list and checks
* that the words that have the MONFLM flag set in their header's attribute
* field have a definition that still matches the checksum stored in the word's
* header. This is meant to be used interactively, as a debugging tool, since
* multitasking is not supported by this implementation. Therefore no status
* value is returned. Feedback will only be provided for corrupted words, in
* the form of a diagnostic message printed to the console.
ICHECK	fcb	6
	fcc	'ICHECK'	( -- )
	fdb	MONITOR
	RFCS
	ldy	DICEND		Upper bound for the code of the last word (exc.)
	ldx	LSTWAD		LAST points to the header of the last word
@icklop	pshs	x		Current word's header address
	lda	,x		Word's header attribute byte to A
	bita	#MONFLM		Is this a monitored word?
	beq	@icknxt		No. Point to the next word
	bsr	HDRSKIP		Skip the header (XT to X), clear A
	bsr	HDRCSUM		Current word's definition's checksum to A
	cmpa	-1,x		Does the checksum match the compile time value?
	beq	@icknxt		Yes. Point to the next word
* Current word's definition has been modified since its original definition!
	jsr	PUTCR
	ldx	,s		Current word's header address
	ldb	,x+
	andb	#WRLNMSK
	clra
	jsr	NPUSH		Word's name base address
	tfr	d,x
	jsr	NPUSH		Byte count for TYPE
	RFXT	jsr,TYPE+7	XT for TYPE
	RFXT	jsr,SPACE+8	XT for SPACE
	ldx	#CSUMFLM
	jsr	PUTS		Feedback for checksum failure
@icknxt	puls	x		Current word's header address
	tfr	x,y		Point to the end of the previous word's code
	bsr	HDRSKIP		Skip the header (XT to X), clear A
	ldx	-3,x		Point to the previous header via the backlink
	beq	@ickdon		We've just reached the end of the dictionary
	bra	@icklop
@ickdon	rts

CSUMFLM	fcn	'integrity check failed'
	ENDC			RELFEAT enabled

DO	fcb	$C2		79-STANDARD (REQ142)
	fcc	'DO'
	IFNE	RELFEAT
	fdb	ICHECK
	ELSE
	fdb	RSTRCT
	ENDC
	RFCS
	ldx	DICEND
	leax	3,x
	jsr	RPUSH
	ldx	#DOEX
	stx	2,s		Set as action component
	rts

DOEX	RFXT	jsr,SWAP+7	XT for SWAP
	RFXT	jsr,TOR+5	XT for >R
	RFXT	jmp,TOR+5	XT for >R

LOOP	fcb	$C4		79-STANDARD (REQ124)
	fcc	'LOOP'
	fdb	DO
	RFCS
	ldx	#LOOPEX
LOOP1	ldy	DICEND
	lda	#JSROPC		JSR extended
	jsr	CHKRTS		Check if the final RTS can be omitted
	jsr	VARCON2
	ldx	#BCSOPC		Compile "BCS *+5"
	stx	,y++
	jsr	RPOP
	lda	#JMPOPC
	jsr	VARCON2		Compile "JMP R@"
	sty	DICEND		No action component
	rts

LOOPEX	ldx	#1
	bra	PLOPEX1

PLOOP	fcb	$C5		79-STANDARD (REQ141)
	fcc	'+LOOP'		The sign hdl reqs for REQ124 should apply though
	fdb	LOOP
	RFCS
	ldx	#PLOOPEX
	bra	LOOP1

PLOOPEX	jsr	NPOP
PLOPEX1	tfr	x,w		Increment to W
	jsr	RPOP
	tfr	x,y		Index to Y
	jsr	RPOP		Limit to X
	addr	w,y		Update index
	tste
	bmi	@neginc
	cmpr	y,x
	ble	@done		We're done. Return With CFLAG set
@iter	jsr	RPUSH		Push back the limit
	tfr	y,x
	jsr	RPUSH		Push back the index
	andcc	#^CFLAG		Clear CFLAG
	rts
@neginc	cmpr	y,x
	beq	@iter		79-STANDARD irregular historical precedent
	bmi	@iter
@done	orcc	#CFLAG		Set CFLAG
	rts

UNLOOP	fcb	$46		ANSI (Core)
	fcc	'UNLOOP'
	fdb	PLOOP
	RFCS
	jsr	RPOP		Drop the index from the return stack
	jmp	RPOP		and the loop limit as well

IF	fcb	$C2		79-STANDARD (REQ210)
	fcc	'IF'
	fdb	UNLOOP
	RFCS
	ldy	DICEND
	ldx	#IFEX
	lda	#JSROPC		JSR extended
	jsr	CHKRTS		Check if the final RTS can be omitted
	jsr	VARCON2
	ldx	#BNEOPC		Compile "BNE *+5"
	stx	,y++
	lda	#JMPOPC		JMP extended
	sta	,y+
	tfr	y,x
	jsr	RPUSH
	leay	2,y		Reserve 2 bytes for the jump address
	sty	DICEND
	rts

IFEX	jsr	NPOP
	cmpr	0,x
	rts

* Functionally equivalent to:
* (ANSI) : UNLESS ['] 0= COMPILE, POSTPONE IF ; IMMEDIATE RESTRICT
* or
* (old style) : UNLESS COMPILE NOT [COMPILE] IF ; IMMEDIATE RESTRICT
UNLESS	fcb	$C6		Non-standard (Perl inspired)
	fcc	'UNLESS'
	fdb	IF
	RFCS
	IFNE	RELFEAT
	ldx	#NULP+6		XT for 0=
	ELSE
	ldx	#NULP+5		XT for 0=
	ENDC
	jsr	CMPCOM1
	RFXT	bra,IF+5	XT for IF

ELSE	fcb	$C4		79-STANDARD (REQ167)
	fcc	'ELSE'
	fdb	UNLESS
	RFCS
	ldy	DICEND
	lda	#JMPOPC		JMP extended
	sta	,y+
	leay	2,y
	sty 	DICEND
	jsr	RPOP
	sty	,x		Set actual ELSE jump address
	tfr	y,x
	leax	-2,x
	jmp	RPUSH

THEN	fcb	$C4		79-STANDARD (REQ161)
	fcc	'THEN'
	fdb	ELSE
	RFCS
	ldy	DICEND
	jsr	RPOP
	sty	,x
	rts

EQ	fcb	1		79-STANDARD (REQ173)
	fcc	'='		( N1 N2 -- FLAG )
	fdb	THEN
	RFCS
	MINDREQ	2
	ldq	,u		D:W has N2:N1
	leau	2,u		Drop one cell from the user stack
	tfr	0,x
	cmpr	w,d
	bne	@eq1
	leax	1,x
@eq1	stx	,u		Store in place to FLAG
	rts

DIFF	fcb	2		79-STANDARD (REF)
	fcc	'<>'		( N1 N2 -- FLAG )
	fdb	EQ
	RFCS
	MINDREQ	2
	ldq	,u		D:W has N2:N1
	leau	2,u		Drop one cell from the user stack
	tfr	0,x
	cmpr	w,d
	beq	@diff1
	leax	1,x
@diff1	stx	,u		Store in place to FLAG
	rts

SINFEQ	fcb	2		Non-standard (Not even ANSI!)
	fcc	'<='		Required for the ORNL fixed sieve benchmark
	fdb	DIFF
	RFCS
	MINDREQ	2		At least two cells need to be stacked up
	ldy	,u
	ldx	2,u
	clrd
	cmpr	y,x
	bgt	@sinfq1
	incd
@sinfq1	leau	2,u		Drop one cell
	std	,u
	rts

	IFNE	DEBUG
CC	fcb	2		Non-standard. Used for debugging
	fcc	'CC'
	fdb	SINFEQ
	RFCS
	ldx	CCREG
	jmp	NPUSH
	ENDC

XOR	fcb	3		79-STANDARD (REQ179)
	fcc	'XOR'
	IFNE	DEBUG
	fdb	CC
	ELSE
	fdb	SINFEQ
	ENDC
	RFCS
	MINDREQ	2
	ldd	,u
	ldw	2,u
	eorr	w,d
XOR1	leau	2,u
	std	,u
	rts

OR	fcb	2		79-STANDARD (REQ223)
	fcc	'OR'
	fdb	XOR
	RFCS
	MINDREQ	2
	ldd	,u
	ldw	2,u
	orr	w,d
	bra	XOR1

AND	fcb	3		79-STANDARD (REQ183)
	fcc	'AND'
	fdb	OR
	RFCS
	MINDREQ	2
	ldd	,u
	ldw	2,u
	andr	w,d
	bra	XOR1

COM	fcb	3		79-STANDARD (REF)
	fcc	'COM'
	fdb	AND
	RFCS
COM0	jsr	NPOP
	tfr	x,d
	comd
	tfr	d,x
	UCNPUSH
	rts

INVERT	fcb	6		ANSI (Core)
	fcc	'INVERT'
	fdb	COM
	RFCS
	bra	COM0

ZGREAT	fcb	2		79-STANDARD (REQ118)
	fcc	'0>'
	fdb	INVERT
	RFCS
	jsr	NPOP
	tfr	x,d
	tstd
	ble	@zgrt1
	ldx	#1
	UCNPUSH
	rts
@zgrt1	tfr	0,x
	UCNPUSH
	rts

ZLESS	fcb	2		79-STANDARD (REQ144)
	fcc	'0<'
	fdb	ZGREAT
	RFCS
	jsr	NPOP
	tfr	x,d
	tstd
	bge	@zlss1
	ldx	#1
	UCNPUSH
	rts
@zlss1	tfr	0,x
	UCNPUSH
	rts

NULP	fcb	2		79-STANDARD (REQ180)
	fcc	'0='
	fdb	ZLESS
	RFCS
	jsr	NPOP
	tfr	x,d
	tfr	0,x
	tstd
	beq	@nulp2
@nulp1	UCNPUSH
	rts
@nulp2	leax	1,x
	bra	@nulp1

NOT	fcb	3		79-STANDARD (REQ165)
	fcc	'NOT'
	fdb	NULP
	RFCS
	RFXT	bra,NULP+5	XT for 0=

USUP	fcb	2		ANSI (Core Ext)
	fcc	'U>'
	fdb	NOT
	RFCS
	jsr	CMP2
	bls	@usup1
	leax	1,x
@usup1	UCNPUSH
	rts

UINF	fcb	2		79-STANDARD (REQ150)
	fcc	'U<'
	fdb	USUP
	RFCS
	jsr	CMP2
	bhs	@uinf1		Z is set
	leax	1,x
@uinf1	UCNPUSH
	rts

* Functionally: : WITHIN OVER - >R - R> U< ;
WITHIN	fcb	6		ANSI (Core)
	fcc	'WITHIN'	( n1 n2 n3 -- flag )
	fdb	UINF
	RFCS
	RFXT	jsr,OVER+7	XT for OVER
	RFXT	jsr,MINUS+4	XT for -
	RFXT	jsr,TOR+5	XT for >R
	RFXT	jsr,MINUS+4	XT for -
	RFXT	jsr,RFROM+5	XT for R>
	RFXT	bra,UINF+5	XT for U<

SUP	fcb	1		79-STANDARD (REQ102)
	fcc	'>'
	fdb	WITHIN
	RFCS
	jsr	CMP2
	ble	@sup1
	leax	1,x
@sup1	UCNPUSH
	rts

INF	fcb	1		79-STANDARD (REQ139)
	fcc	'<'
	fdb	SUP
	RFCS
	jsr	CMP2
	bge	@inf1
	leax	1,x
@inf1	UCNPUSH
	rts
 
MAX	fcb	3		79-STANDARD (REQ218)
	fcc	'MAX'
	fdb	INF
	RFCS
	jsr	NPOP
	tfr	x,y
	jsr	NPOP
	cmpr	y,x
	bge	@pshrv1
	tfr	y,x
@pshrv1	UCNPUSH
	rts

MIN	fcb	3		79-STANDARD (REQ127)
	fcc	'MIN'
	fdb	MAX
	RFCS
	jsr	NPOP
	tfr	x,y
	jsr	NPOP
	cmpr	y,x
	ble	@pshrv2
	tfr	y,x
@pshrv2	UCNPUSH
	rts

ABS	fcb	3		79-STANDARD (REQ108)
	fcc	'ABS'
	fdb	MIN
	RFCS
	jsr	NPOP
	tfr	x,d
	tstd
	bpl	@abs1
	negd
	tfr	d,x
@abs1	UCNPUSH
	rts

NEGATE	fcb	6		79-STANDARD (REQ177)
	fcc	'NEGATE'
	fdb	ABS
	RFCS
	MINDREQ	1		One cell needs to be stacked up
	ldd	,u
	negd
	std	,u
	rts

BEGIN	fcb	$C5		79-STANDARD (REQ147)
	fcc	'BEGIN'
	fdb	NEGATE
	RFCS
	ldx	DICEND
	jmp	RPUSH

AGAIN	fcb	$C5		79-STANDARD (REF114)
	fcc	'AGAIN'
	fdb	BEGIN
	RFCS
	jsr	RPOP
	tfr	x,y
	ldx	DICEND
	lda	#JMPOPC		JMP extended
	sta	,x+
	sty	,x++
	stx	DICEND
	lda	#2
	sta	RTSREMV		Trigger the RTS removal optimization
	rts

# The standard does not require this as being immediate but I do.
EXIT	fcb	$C4		79-STANDARD (REQ117)
	fcc	'EXIT'
	fdb	AGAIN
	RFCS
	ldx	DICEND
	lda	#RTSOPC		RTS inherent
	sta	,x+
	stx	DICEND
	rts

UNTIL	fcb	$C5		79-STANDARD (REQ237)
	fcc	'UNTIL'
	fdb	EXIT
	RFCS
	ldy	DICEND
	lda	#JSROPC		JSR extended
	ldx	#IFEX
	jsr	CHKRTS		Check if the final RTS can be omitted
	jsr	VARCON2
	ldx	#BNEOPC		Compile "BNE *+5"
	stx	,y++
	lda	#JMPOPC		JMP extended
	sta	,y+
	jsr	RPOP
	stx	,y++
	sty	DICEND
	rts

END	fcb	$C3		79-STANDARD (REF224)
	fcc	'END'
	fdb	UNTIL
	RFCS
	RFXT	bra,UNTIL+8	XT for UNTIL

WHILE	fcb	$C5		79-STANDARD (REQ149)
	fcc	'WHILE'
	fdb	END
	RFCS
	RFXT	jmp,IF+5	XT for IF

REPEAT	fcb	$C6		79-STANDARD (REQ120)
	fcc	'REPEAT'
	fdb	WHILE
	RFCS
	jsr	RPOP
	pshs	x
	jsr	RPOP
	ldy	DICEND
	lda	#JMPOPC		JMP extended
	jsr	VARCON2
	puls	x
	sty	,x
	sty	DICEND
	rts

RFROM	fcb	$42		79-STANDARD (REQ110)
	fcc	'R>'
	fdb	REPEAT
	RFCS
	jsr	RPOP
	jmp	NPUSH

TOR	fcb	$42		79-STANDARD (REQ200)
	fcc	'>R'
	fdb	RFROM
	RFCS
	jsr	NPOP
	jmp	RPUSH

LEAVE	fcb	$45		79-STANDARD (REQ213)
	fcc	'LEAVE'
	fdb	TOR
	RFCS
	jsr	RPOP
	jsr	RPOP
	jsr	RPUSH
	jmp	RPUSH

INDI	fcb	$41		79-STANDARD (REQ136)
	fcc	'I'
	fdb	LEAVE
	RFCS
	ldb	#1		1 means top of the stack
RPICKN	lda	RDEPTH
	cmpr	b,a
	bcs	@rpick1
	ldx	RSP
	decb			Minus 1, unlike in the Z80 implementation
	lslb			Times 2
	abx
	ldx	,x
	jmp	NPUSH
@rpick1	ldb	#8		Return stack underflow
	jsr	ERRHDLR		No return

RFETCH	fcb	$42		79-STANDARD (REQ228)
	fcc	'R@'
	fdb	INDI
	RFCS
	RFXT	bra,INDI+4	XT for I

INDIP	fcb	$42		79-STANDARD (REF)
	fdb	$4927
	fdb	RFETCH
	RFCS
	ldb	#2
	bra	RPICKN

INDJ	fcb	$41		79-STANDARD (REQ225)
	fcc	'J'
	fdb	INDIP
	RFCS
	ldb	#3
	bra	RPICKN

INDJP	fcb	$42		Non-standard
	fdb	$4A27
	fdb	INDJ
	RFCS
	ldb	#4
	bra	RPICKN

INDK	fcb	$41		79-STANDARD (REF)
	fcc	'K'
	fdb	INDJP
	RFCS
	ldb	#5
	bra	RPICKN

QUIT	fcb	4		79-STANDARD (REQ211)
	fcc	'QUIT'
	fdb	INDK
	RFCS
	clr	USTATE+1
	RFXT	jsr,RCLR+7	XT for RCLR
	lds	#RAMSTRT+RAMSIZE Reset system stack pointer
	jsr	PUTCR
	jmp	INTERP

ABORT	fcb	5		79-STANDARD (REQ101)
	fcc	'ABORT'
	fdb	QUIT
	RFCS
	RFXT	jsr,NCLR+7	XT for NCLR
	RFXT	jsr,RCLR+7	XT for RCLR
	ldb	#3
	jsr	ERRHDLR		No return

FIND	fcb	4		79-STANDARD (REQ203)
	fcc	'FIND'		( -- XT )
	fdb	ABORT
	RFCS
	tfr	0,y		Default return value is zero
	jsr	BKIN2PT		Derive input stream pointer from BLK, >IN
	tst	,x
	beq	@find1
	jsr	SCNSTOK
	beq	@find1
	jsr	SWDIC
	ldd	TOKENEP
	subd	BSBFADR
	std	UTOIN		Derive >IN from TOKENEP
@find1	tfr	y,x
	jmp	NPUSH

RBRACK	fcb	1		79-STANDARD (REQ126)
	fcc	']'
	fdb	FIND
	RFCS
	lda	#1
	sta	USTATE+1
	rts

* Note: the standard does not mandate that this primitive be executed in
* definitions only. IMHO, it ought to, Therefore I am forcing the C bit here.
LBRACK	fcb	$C1		79-STANDARD (REQ125)
	fcc	'['
	fdb	RBRACK
	RFCS
	clr	USTATE+1
	rts

BKCOMP	fcb	$C9		79-STANDARD (REQ179)
	fcc	'[COMPILE]'
	fdb	LBRACK
	RFCS
	jsr	BKIN2PT		Derive input stream pointer from BLK, >IN
	tst	,x
	bne	@bkcmp2
@bkcmp1	ldb	#5		Missing word name
	jsr	ERRHDLR		No return
@bkcmp2	jsr	SCNSTOK
	beq	@bkcmp1
	jsr	SWDIC
	bne	@bkcmp3		Word found. Code address returned in Y
	ldx	TOKENSP
	ldb	#2		Undefined (X points to the offending word)
	jsr	ERRHDLR		No return
@bkcmp3	sty	2,s		Set as action component
	ldd	TOKENSP		Updated by SWDIC if the word was found
	subd	BSBFADR
	std	UTOIN
	rts

* Functionally: : ['] FIND [COMPILE] LITERAL ; IMMEDIATE RESTRICT
BKQUOT	fcb	$C3		ANSI (Core)
	fcb	$5B,$27,$5D
	fdb	BKCOMP
	RFCS
	RFXT	bsr,FIND+7	XT for FIND
* Data stack topmost cell has the target word address.
	RFXT	jmp,LITERAL+10	XT for LITERAL

POSTPON	fcb	$C8		ANSI (Core)
	fcc	'POSTPONE'	( -- ) as an alias for [COMPILE]
	fdb	BKQUOT
	RFCS
	RFXT	bra,BKCOMP+12	XT for [COMPILE]

	IFEQ	STRCT79
COMPILE	fcb	$47		79-STANDARD (REQ146)
	fcc	'COMPILE'
	fdb	POSTPON
	RFCS
* Return address points to 3 bytes of code (JSR <cfa>) to be inserted HERE and
* to be skipped before returning to the caller. This is not permitted by a
* strict interpretation of the 79-STANDARD, which only provides for a threaded
* implementation (2 bytes payload). For all practical purposes, it works though.
	puls	x
	ldy	DICEND
	lda	,x+		JSR opcode
	sta	,y+
	ldd	,x++
	std	,y++
	sty	DICEND
	tfr	x,pc
	ENDC

* Like the 79-STANDARD COMPILE word, GNU Forth has this as a compile-only word.
* This is a wise choice since it allows us to possibly optimize it.
CMPCOMA	fcb	$48		ANSI (Core Ext)
	fcc	'COMPILE,'	( XT -- )
	IFNE	STRCT79
	fdb	POSTPON
	ELSE
	fdb	COMPILE
	ENDC
	RFCS
	jsr	NPOP		Execution token to X
CMPCOM1	ldy	DICEND
	lda	#JSROPC
	jsr	CHKRTS
	jsr	VARCON2
	sty	DICEND
	rts

* As per the standard, : is not immediate. This allows for further interesting
* developments, like tracing words execution...
COMPC	fcb	$1		79-STANDARD (REQ116)
	fcc	':'
	fdb	CMPCOMA
	RFCS
	lda	#1
	sta	USTATE+1
	clrd
	std	JSRLAST
	lda	#2
	sta	RTSREMV		Optimistic strategy: remove the final RTS
	lda	RDEPTH
	sta	IRDPTH		Meant to check for unbalanced constructs
	jmp	LOCWRT

COMPR	fcb	$C1		79-STANDARD (REQ196)
	fcc	';'
	fdb	COMPC
	RFCS
	lda	RDEPTH		Return stack depth
	cmpa	IRDPTH		Same as when : was entered?
	beq	@compr1
	ldb	#9		Illegal construct if not
	jsr	ERRHDLR		No return
@compr1	clr	USTATE+1	Back to interpretation mode
	ldx	BDICEND
	stx	LSTWAD		Update LAST
	ldx	DICEND
* Optimization: replace the last JSR by a JMP, if possible.
	ldd	JSRLAST
	beq	@compr3		We need an RTS
	leax	-3,x
	cmpx	JSRLAST
	bne	@compr2
	lda	#JMPOPC		JMP extended
	sta	,x
* At this point we still have to emit an RTS unless RTSREMV is 2 or more.
@compr2	leax	3,x
	lda	RTSREMV
	cmpa	#2
	bhs	@compr4		Optimization applies. We have no forward refs
@compr3	lda	#RTSOPC		RTS inherent
	sta	,x+
@compr4
	IFNE	DEBUG
	lda	#ILLOPC		Illegal opcode
	sta	,x+
	ENDC
	stx	DICEND		Update HERE
	leas	4,s		Drop 2 cells from the system stack (see COMP)
	IFNE	RELFEAT
	RFXT	jsr,MONITOR+10	XT for MONITOR. All : words are candidates
*				for integrity check by ICHECK.
	ENDC
	jmp	INTRPRA

RECURSE	fcb	$C7		FORTH-83
	fcc	'RECURSE'
	fdb	COMPR
	RFCS
	ldx	RECADDR		Set up by LOCWRT
	stx	2,s		Set as action component
	rts

FORGET	fcb	6		79-STANDARD (REQ196)
	fcc	'FORGET'
	fdb	RECURSE
	RFCS
	jsr	BKIN2PT		Derive input stream pointer from BLK, >IN
	tst	,x		EOL?
	bne	@frgt2		No
@frgt1	ldb	#5		Missing word name
	jsr	ERRHDLR		No return
@frgt2	jsr	SCNSTOK
	beq	@frgt1		EOL before a non-SP character could be acquired
	jsr	SWDIC		SWDIC uses TOKENSP as input--not X!!
	bne	@frgt3		Word found. XT returned in Y
	ldx	TOKENSP
	ldb	#2		Undefined (X points to the offending word)
	jsr	ERRHDLR		No return
@frgt3	tfr	y,x		Y and X have the word's XT
	cmpy	#RAMFTCH
	beq	@frgt4
	cmpy	#ROMSTRT
	bhs	@frgt4
	IFNE	RELFEAT
	ldx	-3,x		Backlink to X
	stx	LSTWAD		Update LAST
	leax	-4,y		XT-4 to X (1B/attr, 2B/backlink, 1B/checksum)
	ELSE
	ldx	-2,x		Backlink to X
	stx	LSTWAD		Update LAST
	leax	-3,y		XT-3 to X (1B/attr, 2B/backlink)
	ENDC
	clra
	ldb	CURTOKL		Token name length to D
	subr	d,x		Substract word length
	stx	DICEND		Update HERE
	ldd	TOKENSP		Set by SWDIC to point to the end of the token
	subd	BSBFADR
	std	UTOIN		Derive >IN from TOKENSP
	rts
@frgt4	ldb	#11		Word is unforgettable
	jsr	ERRHDLR		No return

EXCT	fcb	7		79-STANDARD (REQ163)
	fcc	'EXECUTE'
	fdb	FORGET
	RFCS
	jsr	NPOP
	cmpr	0,x		Although the standard does not specify that
	beq	@exct1		a NUL address should trigger an error, I do
	tfr	x,pc
@exct1	ldb	#13		Illegal argument
	jsr	ERRHDLR		No return

BYE	fcb	3		Non-standard
	fcc	'BYE'
	fdb	EXCT
	RFCS
	jmp	RSTHDL

BKCHAR	fcb	$C6		ANSI (Core)
	fcc	'[CHAR]'
	fdb	BYE
	RFCS
	RFXT	bsr,CHAR+7	XT for CHAR
	RFXT	jmp,LITERAL+10	XT for LITERAL

CHAR	fcb	4		ANSI (Core)
	fcc	'CHAR'
	fdb	BKCHAR
	RFCS
	jsr	BKIN2PT		Derive input stream pointer from BLK, >IN
@char1	jsr	SCNSTOK		X points to the beginning of the character
	beq	@chrerr
	ldb	,x
	clra
	tfr	d,x
	jsr	NPUSH
	ldx	TOKENSP		Set by SCNSTOK
	jsr	SCNETOK
	tfr	x,d		TOKENEP
	subd	BSBFADR
	std	UTOIN		Derive >IN from TOKENEP
	rts
@chrerr	ldb	#13		Illegal argument
	jsr	ERRHDLR
* No return.

* Hairy code but working.
WORD	fcb	4		79-STANDARD (REQ181)
	fcc	'WORD'		( char -- addr )
	fdb	CHAR
	RFCS
	jsr	NPOP
	tfr	x,w		F has the delimiter ASCII code
	ldy	DICEND		The counted string returned is stored at HERE
	pshs	y
	clr	,y+		Initialize its length
	jsr	BKIN2PT		Derive input stream pointer from BLK, >IN
	tst	,x		EOL reached?
	bne	@word1		No, proceed
@word0	puls	x
	UCNPUSH			Push back HERE
	rts
@word1	jsr	SCNSTOK		Scan for the next non-space character
	beq	@word5		EOL reached, this is the end
	lda	,x+		First non-space character
	cmpr	f,a		Leading delimiter matched?
	bne	@word3		No
@word2	lda	,x+		Acquire next character from the input stream
@word3	sta	,y+
	beq	@word4		EOL reached
	cmpr	f,a		Trailing delimiter?
	beq	@word5
	inc	[,s]		Increment string length
	bra	@word2
@word4	leax	-1,x		EOL reached
@word5	tfr	x,d		Pointing one char after the delimiter or to NUL
	subd	BSBFADR
	std	UTOIN
	bra	@word0

LPAR	fcb	$81		79-STANDARD (REQ122)
	fcc	'('
	fdb	WORD
	RFCS
	jsr	BKIN2PT		Derive input stream pointer from BLK, >IN
@lpar1	lda	,x+
	beq	@lparx		Input stream exhausted before ) is matched
	cmpa	#')
	bne	@lpar1
	tfr	x,d		Just matched )
	subd	BSBFADR
	std	UTOIN		Update >IN
	rts
@lparx	ldb	#12		Missing delimiter
	jsr	ERRHDLR		No return

SOURCE	fcb	6		ANSI (Core)
	fcc	'SOURCE'	( -- baseaddr charcount )
	fdb	LPAR
	RFCS
	ldx	BSBFADR
	jsr	NPUSH
	ldx	#BLKSIZ		Non-zero block size
	ldd	UBLK
	bne	@srcdon		Current BLK is NZ
	clra
	ldb	CMDLNSZ		Character count entered through GETS in INTERP
	tfr	d,x
@srcdon	jmp	NPUSH

* This is a straightforward implementation borrowed from GNU Forth 'see /':
* : \  
*   BLK @ 
*   IF     >IN @ C/L / 1+ C/L * >IN ! EXIT 
*   THEN 
*   SOURCE >IN ! DROP ; IMMEDIATE
* However since C/L (number of columns per line) is 64 (a power of 2), things
* can be coded in a more compact manner as: >IN @ 63 COM AND 64 + >IN !
BKSLSH	fcb	$81		ANSI (Block Ext)
	fcb	$5C		\ ( -- )
	fdb	SOURCE
	RFCS
	ldd	UBLK		BLK @ to D
	beq	@comser		We operate from block 0: the console
	ldd	UTOIN		>IN @
	andb	#^$3F		Point to the beginning of the line
	addb	#$40		next line
	adca	#0
	std	UTOIN		>IN !
	rts			EXIT
@comser	RFXT	bsr,SOURCE+9	XT for SOURCE
	jsr	NPOP
	stx	UTOIN		>IN !
	RFXT	jmp,DROP+7	XT for DROP
	ENDC

PSTR	fcb	$82		79-STANDARD (REQ133)
	fcc	'."'
	fdb	BKSLSH
	RFCS
	jsr	BKIN2PT		Derive input stream pointer from BLK, >IN
	tst	,x+
	beq	@pstr2		Met the end of the input stream before "
	stx	TOKENSP		Beginning of string address
@pstr1	lda	,x+
	beq	@pstr2
	cmpa	#'"
	beq	@pstr3
	bra	@pstr1
@pstr2	ldb	#12		Missing delimiter
	jsr	ERRHDLR		No return
@pstr3	stx	TOKENEP		X points to the character following "
	clr	-1,x		Overwrite the trailing " with NUL
	tfr	x,d
	subd	BSBFADR
	std	UTOIN		Derive >IN from TOKENEP
	tst	USTATE+1
	bne	PSTRCMP
	tfr	x,y		Y has TOKENEP
	ldx	TOKENSP
	jsr	PUTS
	tfr	y,x
@pstr4	lda	#'"
	sta	-1,x		Restore the trailing "
	rts
PSTRCMP	ldy	DICEND		We are compiling
	lda	#LDXOPC		LDX immediate
	sta	,y+
	leax	5,y
	stx	,y++
	lda	#JMPOPC		JMP extended
	sta	,y+
	pshs	y		Jump address beyond the string
	leay	2,y
	ldx	TOKENSP
	ldd	TOKENEP
	subr	x,d
	tfr	d,w
	tfm	x+,y+
	sty	DICEND
	puls	x
	sty	,x		Install JMP address
	ldx	#PUTS
	stx	2,s		Set as action component
	ldx	TOKENEP
	bra	@pstr4

* Transactional behaviour is guaranteed here. What this means is that the
* operation will preserve the data stack contents, should insufficient
* parameters be supplied. It is a very desirable feature (for debugging
* purposes) which will be generalized to a number of other words. As an
* aside, it also allows us to access the data stack as directly indexed
* through the 6309 U register, resulting in better performance.
DPLUS	fcb	2		79-STANDARD (REQ241)
	fcc	'D+'		( d2 d1 -- d1+d2--signed )
	fdb	PSTR		Initially ( L2 H2 L1 H1)
	RFCS
	MINDREQ	4		Make sure we have at least 4 cells stacked up
* At this point sufficient stack depth has been assessed. Let's rock and roll!
@stkok	RFXT	jsr,ROT+6	XT for ROT ( L2 L1 H1 H2 )
	ldd	4,u		L1
	addd	6,u		L2
	std	6,u		d1+d2 least significant cell
	ldd	2,u		H1
	adcd	#0		Carry from least significant cell addition
	addd	,u		H2
	std	4,u		d1+d2 most significant cell
	leau	4,u		Drop 2 cells from the data stack
	rts

DNEG	fcb	7		79-STANDARD (REQ245)
	fcc	'DNEGATE'
	fdb	DPLUS
	RFCS
	MINDREQ	2		We need at least 2 cells stacked up
	ldw	2,u		Least significant cell
	comw
	ldd	,u		Most significant cell
	comd
	addw	#1
	adcd	#0		Propagate carry flag
	stq	,u		Store the result back to the data stack
	rts

DZEQ	fcb	3		79-STANDARD (double number extension)
	fcc	'D0='		( d -- flag )
	fdb	DNEG
	RFCS
	RFXT	jsr,OR+5	XT for OR
	RFXT	jmp,NULP+5	XT for 0=

* In essence:
* : D< DNEGATE D+ NIP [ HEX ] 8000 AND IF 1 ELSE 0 THEN ;
DLESS	fcb	2		79-STANDARD (REQ244)
	fcc	'D<'		( d1 d2 -- flag )
	fdb	DZEQ
	RFCS
	RFXT	bsr,DNEG+10	XT for DNEGATE
	RFXT	bsr,DPLUS+5	XT for D+
	clra
	sta	2,u
	ldb	,u
	blt	@setto1
@setrv	sta	3,u
	leau	2,u
	rts
@setto1	inca
	bra	@setrv

TWOOVER	fcb	5		79-STANDARD (double number extension)
	fcc	'2OVER'		( d1 d2 -- d1 d2 d1 )
	fdb	DLESS
	RFCS
	MINDREQ	4
	ldq	4,u		D:W has MSC:LSC of D1
	tfr	w,x
	jsr	NPUSH
	tfr	d,x
	jmp	NPUSH

TWOSWAP	fcb	5		79-STANDARD (double number extension)
	fcc	'2SWAP'		( d1 d2 -- d2 d1 )
	fdb	TWOOVER
	RFCS
	MINDREQ	4		At least four cells must be stacked up
	ldx	4,u		D1 most significant cell
	ldy	6,u		D1 least significant cell
	ldq	,u		D:W has MSC:LSC of D2
	stq	4,u
	stx	,u
	sty	2,u
	rts

TWODROP	fcb	5		79-STANDARD (double number extension)
	fcc	'2DROP'		( d -- )
	fdb	TWOSWAP
	RFCS
	MINDREQ	2		At least two cells must be stacked up
	leau	4,u
	rts

TWODUP	fcb	4		79-STANDARD (double number extension)
	fcc	'2DUP'		( double -- double double )
	fdb	TWODROP
	RFCS
	MINDREQ	2
	ldq	,u		D:W has MSC:LSC of DOUBLE
	tfr	w,x
	jsr	NPUSH
	tfr	d,x
	jmp	NPUSH

TWOSTOR	fcb	2		79-STANDARD (double number extension)
	fcc	'2!'		( double addr -- )
	fdb	TWODUP
	RFCS
	MINDREQ	3
	ldq	2,u		DOUBLE to D:W
	stq	[,u]		Store DOUBLE to ADDR
	leau	6,u		Drop three cells from the user stack
	rts

TWOFTCH	fcb	2		79-STANDARD (double number extension)
	fcc	'2@'		( dbladdr -- double )
	fdb	TWOSTOR
	RFCS
	MINDREQ	1
	ldq	[,u]		D:W has MSC:LSC of DBLADDR @
	stw	,u		Least significant cell stacked in place
	tfr	d,x		Most significant cell goes through standard push
	jmp	NPUSH

CONVERT	fcb	7		79-STANDARD (REQ195)
	fcc	'CONVERT'	( d1 addr1 -- d2 addr2 )
	fdb	TWOFTCH
	RFCS
	MINDREQ	3		At least 3 cells need to be stacked up on entry
	jsr	CKBASE		Check for supported BASE. No return if not
	ldx	,u		ADDR1 to X
@cvloop	leax	1,x
	ldb	,x
* B has the ASCII representation of something that may or may not be a valid
* digit, expressed in BASE (alias (byte)UBASE+1). If it does, multiply D1 by
* BASE and add that to D1 (aka D2 on exit). Then add DIGIT on the top of it.
	subb	#'0		Minimal ASCII value condition met?
	blo	@cvoor		No. Out of range. ,X cannot be a valid digit
        cmpb    #10
        blo     @cvnolt		No letter in potential BASE
        IFEQ    CSSNTVE
	cmpb	#'A-'0
	blo	@cvoor		Greater than 9 but lower than A
        cmpb    #'a-'0
        blo     @cvisuc		Upper case already
        cmpb    #'z-'0
        bhi     @cvoor		Definitely out of range
	subb    #'a-'A          To upper case
        ENDC
@cvisuc subb    #'A-':          A-Z to number
@cvnolt	cmpb	UBASE+1 	B has a digit. Make sure it's less than BASE
	bhs	@cvoor		Number under scrutiny is >= BASE
	leas	-8,s		Allocate scratch space
	clra
	pshs	d
* System and user stack structures are as follows:
* ,s	current digit (1 cell)
* 2,s	D0*B (1 cell)
* 4,s   D1*B (1 cell)
* 6,s   D2*B (1 cell)
* 8,s   D3*B (1 cell)
* 2,u	D1H most significant cell
* 4,u	D1L least significant cell
	tfr	u,v		Backup U
	leau	6,u		Point one byte after D1LL
	leay	2,s		Point to D0*B
	lde	#4		Four products to go through
@cvmul	lda	,-u
	ldb	UBASE+1
	mul
	std	,y++
	dece
	bne	@cvmul
	tfr	v,u		Restore U
	lda	3,s
	sta	5,u		D1LL
	lda	2,s
	adda	5,s
	sta	4,u		D1LH
	lda	4,s
	adca	7,s
	sta	3,u		D1HL
	lda	6,s
	adca	9,s
	sta	2,u		D1HH
	ldd	4,u		D1L
	addd	,s		DIGIT
	std	4,u
	ldd	2,u		D1H
	adcd	#0		Potential carry from lower cell
	std	2,u
	leas	10,s		Release scratch space
	bra	@cvloop		Here we go again
@cvoor	stx	,u		Update ADDR2
	rts

CVTE	fcb	2
	fcc	'#>'
	fdb	CONVERT
	RFCS
	jsr	NPOP
	jsr	NPOP		Drop 2 cells from the data stack
	ldx	#PADBUF
	UCNPUSH
	jsr	SLEN
	tfr	w,x
	UCNPUSH
	rts

SIGN	fcb	4
	fcc	'SIGN'
	fdb	CVTE
	RFCS
	jsr	NPOP
	tfr	x,d
	tstd
	bge	@sign1
	ldb	#'-
	jmp	INSBPAD
@sign1	rts

HOLD	fcb	4
	fcc	'HOLD'
	fdb	SIGN
	RFCS
	jsr	NPOP
	tfr	x,d
	jmp	INSBPAD		B is inserted at the beginning of PAD.

SHARPS	fcb	2
	fcc	'#S'
	fdb	HOLD
	RFCS
	lda	#1
	sta	CVTFCN		CVT function #1 is #S
	bra	CVT0

* Unsigned double on the top of the data stack gets divided by BASE.
* The division algorithm implemented here is the binary long division.
* See https://en.wikipedia.org/wiki/Division_algorithm for more information.
* Remainder (converted to a character) gets prepended to PAD.
CVT	fcb	1
	fcc	'#'
	fdb	SHARPS
	RFCS
	clr	CVTFCN		CVT function 0 is #
CVT0	jsr	NPOP
	tfr	x,w
	jsr	NPOP
CVT1	pshs	x		Numerator least significant cell
	pshsw			Numerator most significant cell
	clrd
	pshs	d		Quotient low
	pshs	d		Quotient high
	pshs	d		Bitmask	low
	ldw	#$8000		Bitmask	high
	pshsw
	pshs	d
* Stack structure:
* 1,s	remainder		8 bits (high order byte is zero and unused)
* 2,s	Bitmask high		16 bits
* 4,s	Bitmask low		16 bits
* 6,s	Quotient high		16 bits
* 8,s	Quotient low		16 bits
* 10,s	Numerator high		16 bits
* 12,s	Numerator low		16 bits
* Denominator is at UBASE+1	8 bits
	ldf	#31		32 bits to go
@cvt1	lsl	1,s		R := R << 1
* We need to extract bit <f> from the numerator.
	leax	10,s		Numerator MSB address
	tfr	f,a		Not to alter the stack structure
	lsra			OffsetX: 3 - F >> 3
	lsra
	lsra
	ldb	#3
	subr	a,b
	abx
	lda	,x
	tfr	f,b		Bitno: F & 7
	andb	#7
* At this point, A has the data we're interested in. B has the bit number.
@cvtex	tstb
	beq	@cvtfnd		Bit 0 of A has the data
	lsra
	decb
	bra	@cvtex
@cvtfnd	anda	#1
	ora	1,s		R(0) := N(i)
	sta	1,s		Update the remainder
	cmpa	UBASE+1
	blo	@cvt5
	suba	UBASE+1
	sta	1,s		R := R - D
* Q(i) := 1 (use the bitmask).
	ldd	6,s		Quotient high
	ord	2,s		Bitmask high
	std	6,s
	ldd	8,s		Quotient low
	ord	4,s		Bitmask low
	std	8,s
* Shift the bitmask 1 bit right.
@cvt5	ldd	2,s
	lsrd
	std	2,s
	ldd	4,s
	rord
	std	4,s
	decf
	bge	@cvt1
* Convert the remainder to a digit expressed in BASE.
	ldb	1,s
	lda	#'0
	cmpb	#10             B has the digit we want converted to BASE
	bcs	@cvtdgt
	lda	#'A-10
@cvtdgt	addr	a,b
	bsr	INSBPAD         Prepend B to the string currently in PAD
	tst	CVTFCN
	beq	@cvtend		Function 0 is straight #, i.e. we're done here
* Function 1 is #S, we iterate unless the quotient is 0.
	ldd	8,s
	ord	6,s
	beq	@cvtend
	ldx	8,s		New numerator low
	ldw	6,s		New numerator high
	leas	14,s		Discard the stack frame
	jmp	CVT1		And go at it again
* Push back the quotient on the data stack (low then high cell).
@cvtend	ldx	8,s
	UCNPUSH
	ldx	6,s
	leas	14,s		Discard the stack frame
	UCNPUSH
	rts

* Insert the character in B in front of the string at PADBUF.
INSBPAD	ldx	#PADBUF
	jsr	SLEN
	addr	w,x		X points to the PAD string's NUL terminator
	incw			Include the terminator
	leay	1,x
	tfm	x-,y-
	stb	PADBUF
	rts

CVTB	fcb	2
	fcc	'<#'
	fdb	CVT
	RFCS
	jsr	CKBASE		Sanity check. Base can be altered at any time
	clr	PADBUF
	rts

DOT	fcb	1		79-STANDARD (REQ193)
	fcc	'.'
	fdb	CVTB
	RFCS
	lda	#1
	sta	CVISSGN		Force a signed number conversion
ptop0	jsr	NPOP
	jsr	CVNSTR
	ldx	#TBUFF
	lda	#SP
@ptop1	cmpa	,x+		Skip leading spaces
	beq	@ptop1
	leax	-1,x		Point to actual string start address
	jsr	PUTS
	jmp	PUTCH		Extra space after printing a number

UDOT	fcb	2		79-STANDARD (REQ106)
	fcc	'U.'
	fdb	DOT
	RFCS
	clr	CVISSGN		Conversion is unsigned
	bra	ptop0

DOTR	fcb	2		79-STANDARD (REF)
	fcc	'.R'
	fdb	UDOT
	RFCS
	lda	#1
	sta	CVISSGN
DOTR0	jsr	NPOP
	pshs	x		Length parameter value
	jsr	NPOP
	jsr	CVNSTR
	ldx	#TBUFF
	lda	#SP
@dotr1	cmpa	,x+
	beq	@dotr1		Skip leading spaces
	leax	-1,x
	jsr	SLEN		String length is returned in W
	puls	d
	exg	d,w
	subr	d,w
	ble	@dotr3
	lda	#SP
@dotr2	tstw
	beq	@dotr3
	jsr	PUTCH
	decw
	bra	@dotr2
@dotr3	jmp	PUTS

UDOTR	fcb	3		79-STANDARD (REF216)
	fcc	'U.R'
	fdb	DOTR
	RFCS
	clr	CVISSGN
	bra	DOTR0

BL	fcb	2		79-STANDARD (REF176)
	fcc	'BL'
	fdb	UDOTR
	RFCS
	ldx	#SP
	jmp	NPUSH

SPACE	fcb	5		79-STANDARD (REQ232)
	fcc	'SPACE'
	fdb	BL
	RFCS
	lda	#SP
	jmp	PUTCH

SPACES	fcb	6		79-STANDARD (REQ231)
	fcc	'SPACES'
	fdb	SPACE
	RFCS
	jsr	NPOP
	tfr	x,w
	lda	#SP
@spcs1	jsr	PUTCH
	decw
	bne	@spcs1
	rts

PAGE	fcb	4		79-STANDARD (REF)
	fcc	'PAGE'
	fdb	SPACES
	RFCS
	lda	#FF
	jmp	PUTCH

CRLF	fcb	2		79-STANDARD (REQ160)
	fcc	'CR'
	fdb	PAGE
	RFCS
	jmp	PUTCR

PAD	fcb	3		79-STANDARD (REQ226)
	fcc	'PAD'
	fdb	CRLF
	RFCS
	ldx	#PADBUF
	jmp	NPUSH

TYPE	fcb	4		79-STANDARD (REQ222)
	fcc	'TYPE'		( addr bcount -- )
	fdb	PAD
	RFCS
	jsr	NPOP		Character count (signed)
	tfr	x,w
	jsr	NPOP		Buffer address
	tstw			
@type0	bgt	@type1
	rts
@type1	lda	,x+
	jsr	PUTCH
	decw
	bra	@type0

COUNT	fcb	5		79-STANDARD (REQ159)
	fcc	'COUNT'
	fdb	TYPE
	RFCS
	jsr	NPOP
	ldb	,x+
	UCNPUSH			B is preserved
	clra
	tfr	d,x
	jmp	NPUSH

DASHTR	fcb	9		79-STANDARD (REQ148)
	fcc	'-TRAILING'	( addr n1 -- addr n2 )
	fdb	COUNT
	RFCS
	jsr	NPOP		N1 to X
	tfr	x,d		N1 to D
	jsr	NPOP		ADDR to X
	tfr	x,y		Backup to I
	tstd			Input character count (N1)
	blt	@invpar		Cannot be < 0
	leax	d,x		X has ADDR+N1
	tfr	d,w		W has N1
@cknxtb	tstw
	beq	@ckdone
	lda	,-x
	decw
	cmpa	#SP
	beq	@cknxtb		Iterate over to the previous byte
	incw
@ckdone	tfr	y,x
	UCNPUSH			String base address
	tfr	w,x
	UCNPUSH			Updated character count
	rts
@invpar	ldb	#13		Invalid parameter
	jsr	ERRHDLR		No return

EXPECT	fcb	6		79-STANDARD (REQ189)
	fcc	'EXPECT'	( addr count -- )
	fdb	DASHTR
	RFCS
	jsr	NPOP
	tfr	x,d		Buffer length to B
	jsr	NPOP		Buffer address to X. B is preserved
	tstb
	beq	@expct1
	incb			Account for the NUL terminator
@expct1	jmp	GETS

TERPRET	fcb	$49		79-STANDARD (REF) I make this compile time only
	fcc	'INTERPRET'	( -- )
	fdb	EXPECT
	RFCS
* Obtain a base buffer address based on the value of BLK.
	ldd	UBLK
	bne	@notser
	ldx	#CMDBUF		Base buffer address for serial line input
	bra	@gocmd
* BLK is NZ, map the block in memory.
@notser	tfr	d,x		Block number to X
	jsr	NPUSH
	RFXT	jsr,BLOCK+8	XT for BLOCK. Map the block in
	jsr	NPOP		Retrieve buffer address
* Note: >IN is supposed to have been set by the caller!
@gocmd	stx	BSBFADR
	jmp	_INTERP		Finally invoke _INTERP.

LOAD	fcb	4		79-STANDARD (REQ202)
	fcc	'LOAD'		( blk -- )
	fdb	TERPRET
	RFCS
	jsr	NPOP
	cmpr	0,x
	bne	@ctnued
	rts			Block 0 is _not_ loadable
@ctnued	pshs	x
	ldx	UBLK
	jsr	RPUSH		Push BLK on the return stack
	ldx	UTOIN
	jsr	RPUSH		Push >IN on the return stack
	puls	x
	stx	UBLK		Update BLK with the LOAD argument
	clrd
	std	UTOIN		Clear >IN
* Map the new BLK in, interpret code from there.
	RFXT	bsr,TERPRET+12	XT for INTERPRET
	jsr	RPOP
	stx	UTOIN		Restore >IN from the return stack
	jsr	RPOP
	stx	UBLK		Restore BLK from the return stack
	jmp	BLK2BB		Update BSBFADR based on the new BLK value

THRU	fcb	4		79-STANDARD (REF)
	fcc	'THRU'		( lowblk highblk -- )
	fdb	LOAD
	RFCS
	jsr	NPOP
	tfr	x,y		Y has highblk
	jsr	NPOP		X has lowblk -- both are unsigned numbers
@thrlop	cmpr	y,x
	bls	@cont
	rts
@cont	UCNPUSH
	pshs	x
	RFXT	bsr,LOAD+7	XT for LOAD
	puls	x
	leax	1,x
	bra	@thrlop

NXTBLK	fcb	$83		79-STANDARD (REF131)
	fcc	'-->'		( -- )
	fdb	THRU
	RFCS
	ldx	UBLK
	leax	1,x
NXTBLK1	ldd	UBLK
	bne	@nfrmb0		Not invoked from block 0 (the console)
* --> or CONTINUED are being invoked from the console. Flag that condition
* as a hint to the interpreter so that feedback is provided even if we are
* back from a block.
	inca			1 to A
	sta	NBCTFB0
@nfrmb0	stx	UBLK		Update BLK
	clrd
	std	UTOIN		0 >IN !
* Map the new BLK in, interpret code from there.
	RFXT	jmp,TERPRET+12	XT for INTERPRET

CONTIND	fcb	$89		79-STANDARD (REF)
	fcc	'CONTINUED'	( nextblk -- )
	fdb	NXTBLK
	RFCS
	jsr	NPOP		NEXTBLK to X
	cmpr	0,x		Cannot interpret from block 0!
	bne	NXTBLK1
	ldb	#13		Illegal argument
	jsr	ERRHDLR		No return

MILLIS	fcb	2		79-STANDARD (REF)
	fcc	'MS'		( mscount -- )
	fdb	CONTIND
	RFCS
	jsr	NPOP
	cmpr	0,x
	bne	MILLIS1
	rts
MILLIS1	ldd	#MSLCNT
@ms2	decd
	bne	@ms2
	leax	-1,x
	bne	MILLIS1
	rts

KEYP	fcb	4		ANSI (Facility)
	fcc	'KEY?'		( -- flag )
	fdb	MILLIS
	RFCS
	tfr	0,x
	lda	#ACIRTS0
	sta	ACIACTL
	lda	#ACIRDRF
	bita	ACIACTL
	beq	@keyp1
	leax	1,x
@keyp1	lda	#ACIRTS1
	sta	ACIACTL
	jmp	NPUSH

KEY	fcb	3		79-STANDARD (REQ100)
	fcc	'KEY'
	fdb	KEYP
	RFCS
	jsr	GETCH
	tfr	a,b
	clra
	tfr	d,x
	jmp	NPUSH

EMIT	fcb	4		79-STANDARD (REQ207)
	fcc	'EMIT'
	fdb	KEY
	RFCS
	jsr	NPOP
	tfr	x,d
	tfr	b,a
	jmp	PUTCH

PLUS	fcb	1		79-STANDARD (REQ121)
	fcc	'+'		( n1 n2 -- sum )
	fdb	EMIT
	RFCS
	MINDREQ 2		We need at least two cells stacked up
	ldd	2,u		N1
	addd	,u		N2
	std	2,u		SUM
	leau	2,u		Drop the top cell
	rts

ONEP	fcb	2		79-STANDARD (REQ107)
	fcc	'1+'
	fdb	PLUS
	RFCS
	MINDREQ	1		We need at least one cell stacked up
	ldd	,u
	incd
	std	,u
	rts

TWOP	fcb	2		79-STANDARD (REQ135)
	fcc	'2+'		( n -- n+2 )
	fdb	ONEP
	RFCS
	MINDREQ	1
	ldd	,u
	addd	#2
	std	,u
	rts

MINUS	fcb	1		79-STANDARD (REQ134)
	fcc	'-'		( n1 n2 -- dif )
	fdb	TWOP
	RFCS
	MINDREQ	2		We need at least two cells stacked up
	ldd	2,u		N1
	subd	,u		N2
	std	2,u		DIF
	leau	2,u		Drop the top cell
	rts

ONEM	fcb	2		79-STANDARD (REQ105)
	fcc	'1-'
	fdb	MINUS
	RFCS
	MINDREQ	1		We need at least one cell stacked up
	ldd	,u
	decd
	std	,u
	rts

TWOM	fcb	2		79-STANDARD (REQ129)
	fcc	'2-'
	fdb	ONEM
	RFCS
	MINDREQ	1		We need at least one cell stacked up
	ldd	,u
	subd	#2
	std	,u
	rts

ZEROL	fcb	1		Non-standard
	fcc	'0'
	fdb	TWOM
	RFCS
	tfr	0,x
	jmp	NPUSH

ONEL	fcb	1		Non-standard
	fcc	'1'
	fdb	ZEROL
	RFCS
	ldx	#1
	jmp	NPUSH

TWOL	fcb	1		Non-standard
	fcc	'2'
	fdb	ONEL
	RFCS
	ldx	#2
	jmp	NPUSH

SHIFT	fcb	5		79-STANDARD (Ref)
	fcc	'SHIFT'
	fdb	TWOL
	RFCS
	MINDREQ	2		Two cells need to be stacked up
	ldw	,u		Shift bitcount
	ldd	2,u		The data itself
@shftlp	tstw
	beq	@shfdon
	blt	@shftrg
	lsld			Shift left (W is positive)
	decw
	bra	@shftlp
@shftrg lsrd			Shift right (W is negative)
	incw
	bra	@shftlp
@shfdon	std	2,u		Return value stored there
	leau	2,u		Drop one cell from the data stack
	rts

* Signed multiplication by hardware support.
MULT	fcb	1		79-STANDARD (REQ138)
	fcc	'*'
	fdb	SHIFT
	RFCS
	MINDREQ	2		Two cells need to be stacked up
	ldd	2,u
	muld	,u
	stw	2,u		Return only the lower 16 bits
	leau	2,u		Drop one cell from the data stack
	rts

TWOTIM	fcb	2		79-STANDARD (REF)
	fcc	'2*'
	fdb	MULT
	RFCS
	MINDREQ	1		One cell needs to be stacked up
	ldd	,u
	asld
	std	,u
	rts

TWODIV	fcb	2
	fcc	'2/'
	fdb	TWOTIM
	RFCS
	MINDREQ	1		One cell needs to be stacked up
	ldd	,u
	asrd
	std	,u
	rts

* /, MOD and /MOD are essentially the same function returning
* the different parts returned by DIVQ.
* We use a global variable to distinguish which functionality
* is being requested:
* 0: return the modulo and the quotient (/MOD).
* 1: return the modulo only (MOD).
* 2: return the quotient only (/).
DIV	fcb	1
	fcc	'/'
	fdb	TWODIV
	RFCS
	lda	#2
	sta	DIVFCN
DIV1	MINDREQ	2
* Division by zero conditions are dealt with through the trap handler.
	clrd
	ldw	2,u		Numerator
* We need a sign extention from W to Q. SEX and SEXW do not cut it.
	tste
	bpl	@div2
	comd
@div2	divq	,u		Quotient is returned in W, modulo in D
	tst	DIVFCN
	bne	@div3
	std	2,u		Function 0: return the modulo and the quotient
	stw	,u
	rts
@div3	leau	2,u		Drop one cell from the data stack
	ldf	DIVFCN
	cmpf	#1
	bne	@div4
	std	,u		Function 1: return only the modulo
	rts
@div4	stw	,u		Function 2: return only the quotient
	rts

MOD	fcb	3
	fcc	'MOD'
	fdb	DIV
	RFCS
	lda	#1
	sta	DIVFCN
	bra	DIV1

MDIV	fcb	4
	fcc	'/MOD'
	fdb	MOD
	RFCS
	clr	DIVFCN
	bra	DIV1

STRSLSH	fcb	2
	fcc	'*/'		( n1 n2 n3 -- n4 [n5] )
	fdb	MDIV
	RFCS
	lda	#1
	sta	STSLFCN
STRSL1	MINDREQ	3
* Division by zero conditions are dealt with through the trap handler.
	ldd	4,u		N1
	muld	2,u		N2
	divq	,u		N3
	leau	2,u
	tst	STSLFCN
	bne	@strsl2		Just the quotient, Ma'am!
	std	2,u		N4: the modulo
	stw	,u		N5: the quotient
	rts
@strsl2	leau	2,u
	stw	,u		N4
	rts

STRSLMD	fcb	5
	fcc	'*/MOD'
	fdb	STRSLSH
	RFCS
	clr	STSLFCN
	bra	STRSL1

* Returns the current value of the S register (informational only).
SYSSTK	fcb	1		Non-standard
	fcc	'S'
	fdb	STRSLMD
	RFCS
	tfr	s,x
	jmp	NPUSH

WORDS	fcb	5		ANSI (Tools)
	fcc	'WORDS'		( -- )
	fdb	SYSSTK
	RFCS
	RFXT	bra,VLIST+8	XT for VLIST

* Differences from the original code:
* - display number in HEX rather than in the current base.
* - dropped feat: the original stuff was interactively paged by 15 line screens.
* - added feat: display code implementation payload.
* - added feat: display the immedediate and define (compile time only) flags.
* - added feat: display the forgettable status (R/W). Everything user
*   defined is forgettable (i.e. RAM resident).
VLIST	fcb	5		Non-standard
	fcc	'VLIST'		( -- )
	fdb	WORDS
	RFCS
	ldx	DICEND
	stx	VLPRVEP		Last word code address + 1
	ldx	LSTWAD
@vlist1	stx	VLPRVHD		Last word header pointer
	ldy	#TBUFF
	ldb	,x+
	pshs	b
	andb	#WRLNMSK	Mask out word length
	clra
	pshs	b		Preserve word length 
	tfr	d,w
	tfm	x+,y+
	puls	a		Restore word length to A
	ldb	#WRLNMSK+1
	subr	a,b		Number of spaces we want to emit
	lda	#SP
@vlist2	sta	,y+
	decb
	bne	@vlist2
	clr	,y+		Terminate formatted output string
	pshs	x		Address of the current word backlink address
	jsr	PUTCR		Carriage return is output before anything else
	ldx	#TBUFF
	jsr	PUTS		Output word name formatted on 17 chars + SP
	ldx	,s
	IFNE	RELFEAT
	leax	3,x		Word XT to X (Skip backlink and checksum)
	ELSE
	leax	2,x		Word XT to X (Skip backlink)
	ENDC
	tfr	x,d		CVNSTR depends on UBASE and we want HEX output
	ldx	#TBUFF
	jsr	HDMP4		So we use trusted debugging code
	ldx	#TBUFF
	jsr	PUTS
* Now to display word implementation length.
* Implementation code length is from ,s+3 (or 2) to VLPRVEP
	lda	#HT
	jsr	PUTCH
	ldx	VLPRVEP
	ldy	,s
	IFNE	RELFEAT
	leay	3,y		Current word code entry address
	ELSE
	leay	2,y		Current word code entry address
	ENDC
	subr	y,x
	tfr	x,d
	ldx	#TBUFF
	jsr	HDMP4
	ldx	#TBUFF
	jsr	PUTS
# Display attribute flags.
	lda	#HT
	jsr	PUTCH
	ldx	#TBUFF
	ldb	2,s		Word attribute to B
	lda	#'-
	bitb	#IMDFLM		Check for immediate
	beq	@vlist4
	lda	#'I
@vlist4	sta	,x+
	lda	#'-
	bitb	#DEFFLM		Check for define (compile time only)
	beq	@vlist5
	lda	#'C
@vlist5	sta	,x+
	IFNE	RELFEAT
	lda	#'-
	bitb	#MONFLM		Check for monitored status
	beq	@vlst51
	lda	#'M
@vlst51	sta	,x+
	ENDC
* Check for forgettable also. In ROM => R else W unless we just processed '@'.
	lda	#'R
	ldy	,s
	IFNE	RELFEAT
	leay	3,y		Word entry point to Y
	ELSE
	leay	2,y		Word entry point to Y
	ENDC
	cmpy	#RAMFTCH	@ in RAM is unforgettable
	beq	@vlist6
	cmpy	#ROMSTRT	as are all ROM resident words
	bcc	@vlist6
	lda	#'W
@vlist6	sta	,x+
	clr	,x
	ldx	#TBUFF
	jsr	PUTS
* The point at which VLPRVHD==WDICSPC is a singularity (RAM to ROM transition).
	ldx	VLPRVHD
	cmpx	#WDICSPC
	bne	@vlist7
	ldx	#QMARK		Word just before @ (ROM flavor)
	stx	VLPRVHD
	ldx	#THEEND
@vlist7	stx	VLPRVEP
	puls	x
	puls	b
	ldx	,x		Point to previous word
	lbne	@vlist1
	rts

STATE	fcb	5
	fcc	'STATE'
	fdb	VLIST
	RFCS
	ldx	#USTATE
	jmp	NPUSH

BASE	fcb	4		79-STANDARD (REQ115)
	fcc	'BASE'
	fdb	STATE
	RFCS
	ldx	#UBASE
	jmp	NPUSH

BIN	fcb	3		Non-standard
	fcc	'BIN'
	fdb	BASE
	RFCS
	ldd	#2
	std	UBASE
	rts

OCTAL	fcb	5		79-STANDARD (REF)
	fcc	'OCTAL'
	fdb	BIN
	RFCS
	ldd	#8
	std	UBASE
	rts

DECIMAL	fcb	7		79-STANDARD (REQ197)
	fcc	'DECIMAL'
	fdb	OCTAL
	RFCS
	ldd	#10
	std	UBASE
	rts

HEX	fcb	3		79-STANDARD (REF162)
	fcc	'HEX'
	fdb	DECIMAL
	RFCS
	ldd	#16
	std	UBASE
	rts

* DUMP is not optimized for speed or clarity but for code compactness and yet
* it remains the largest dictionary entry! It resorts to builtin HEX dump
* primitives that do not depend on the current base (HDMP4, HDMP2).
* DUMP operates on the basis of one line of output to #TBUFF per 16 byte
* (a block) of input. The input buffer is pointed to by D (backed up by Y)
* and the output is pointed to by X. The output (besides the header line)
* comprises 3 distinct areas:
*
* - the base block address. Aligned on a 16 byte address, followed by one #SP.
* - the HEX dump area. Made up of 16 times 2 HEX numbers separated by #SP.
* - the ASCII dump area. Made up of 16 characters.
*
* This results in the following "magical" constants:
*
* #TBUFF+53 points to the beginning of the ASCII dump area.
* #TBUFF+69 points to the end of the output string (to be NUL terminated).
*
DUMP	fcb	4		79-STANDARD (REF123)
	fcc	'DUMP'		( START NBYTES -- )
	fdb	HEX
	RFCS
	jsr	NPOP
	tfr	x,y		NBYTES to Y
	jsr	NPOP
* Input parameter validation.
	cmpr	0,y
	bne	@dmproc		Proceed if NBYTES in non-zero
	rts			A zero byte count is not flagged as an error
@dmproc	bgt	@dmpini
	ldb	#13		Illegal argument (negative)
	jsr	ERRHDLR		No return
* Initialization and header processing.
@dmpini	pshs	y		NBYTES
	pshs	x		START
	leas	-2,s		Allocate storage for STARTSEEN
* Stack structure:
* ,s	STARTSEEN		Set to 1 as soon as D is seen = to START
*				Only the LSB is used (1,s).
* 2,s	START			Lower bound of the area of interest (included)
* 4,s	NBYTES			Byte count to be dumped
	tfr	x,d		START to D
	andb	#$F0		Align to lower 16 byte boundary
	clr	1,s		Clear STARTSEEN until better informed
	jsr	PUTCR		Print CR
	ldf	#SP		Preserved across called subroutines
	ldx	#TBUFF
	tfr	x,v		Invariants: F has #SP, V has #TBUFF
	pshs	d
	jsr	_DMPHDR		D is not preserved
	puls	d
	clr	,x		NUL terminate the current output line
	tfr	v,x		Point back to the beginning of the output buffer
	jsr	PUTS		Print header
* Main loop. Index is D. We output everything to the string pointed by X.
* Occasional resort to #TBUFF + 53 + (B % 15) will be used.
@dmmain	pshs	b
	andb	#$F
	puls	b
	bne	@dmphx		Not a beginning of line
	jsr	PUTCR		Print CR
	jsr	HDMP4		4 hex digit dump of the base block address
	stf	,x+		SPACE after the base address
@dmphx	tfr	d,y		Save D, prepare pointer to actual data
	bsr	_DMPINT
	bne	@dmhx2		We are in the area of interest
@dmspc	lda	#SP
	sta	,x+		Nothing interesting here. Emit three spaces
	sta	,x+
	sta	,x+
	bra	@dmasc		Have #SP printed to the ASCII area
* We are within the area of interest. Perform a regular HEX 2 digit conversion.
@dmhx2	lda	,y
	jsr	HDMP2
	stf	,x+
* Decrement NBYTES, only if it is not already zero.
	pshs	y
	ldy	6,s		Offset plus 2 since we just pushed Y
	beq	@dmrsty		NBYTES is nul
	leay	-1,y		Decrement NBYTES
	sty	6,s
@dmrsty	puls	y
@dmasc	cmpa	#SP
	blo	@dmnprt		Character is not printable
	cmpa	#$7E
	bls	@dmasis
@dmnprt	lda	#'.		Default character when not printable
@dmasis	pshs	x		The ASCII image is the character itself
	ldx	#TBUFF+53	Base address for the ASCII dump area
	andb	#$F		Offset is B modulo 15
	abx			Add offset to base address
	sta	,x+
	puls	x
* If B is #$F, we've just processed the last byte of a block. Print #TBUFF.
	cmpb	#$F
	tfr	cc,e
	bne	@dmctd
	tfr	v,x		#TBUFF to X
	clr	69,x
	jsr	PUTS
@dmctd	tfr	y,d		Restore D
	incd
	tfr	e,cc
	bne	@dmmain		Proceed until end of block is reached
	tst	5,s		NBYTES LSB
	bne	@dmmain
	tst	4,s		NYTES MSB
	bne	@dmmain
	leas	6,s		Free stack storage used by local variables
	rts

* Predicate to indicate whether or not D covers the area of interest.
* Upon return NZ will be set iff we are pointing to the "interesting" area.
_DMPINT	tst	3,s		STARTSEEN?
	bne	@strsen		Yes, check for zero remaining byte count
	cmpd	4,s		START
	bhs	@ststrt		Set STARTSEEN once and for all
@retwz	orcc	#ZFLAG		Set Z flag
	rts
* STARTSEEN is set. Check remaining byte count.
@strsen	tst	7,s		NBYTES LSB
	bne	@retwnz
	tst	6,s		NBYTES MSB
	bne	@retwnz
	bra	@retwz		We are beyond the interesting area
@ststrt	pshs	a
	lda	#1
	sta	4,s		Set STARTSEEN
	puls	a
@retwnz	andcc	#^ZFLAG		Clear Z flag
	rts

* Print the dump header to X.
_DMPHDR	lda	#SP
	sta	,x+
	sta	,x+
	sta	,x+
	sta	,x+
	clrb
@dmhlp1	sta	,x+		Space before every 2 hex digits
	tfr	b,a
	jsr	HDMP2
	lda	#SP
	incb
	cmpb	#$10
	bne	@dmhlp1
	sta	,x+		Space before the ASCII dump
	clrb
@dmhlp2	tfr	b,a
	jsr	HEX1D		Offset to HEX char stored to x+
	incb
	cmpb	#$10
	bne	@dmhlp2
	rts

	IFNE	SSDFEAT
DOTTICK	fcb	2		Non-standard (SwiftForth)
	fcb	$2E,$27		.' ( memaddr -- )
	fdb	DUMP
	RFCS
	jsr	NPOP
	tfr	x,y
	ldx	#HEXBUF
	jsr	FINDSYM
	ldx	#HEXBUF
	jmp	PUTS
	ENDC

* Display an HEX dump of the data stack. In Leo Brodie's "Starting Forth"
* the data stack is printed from the bottom up. So is it here.
DDUMP	fcb	2		ANSI (Optional "Programming tools" word set)
	fcc	'.S'		( memaddr bcount -- )
	IFNE	SSDFEAT
	fdb	DOTTICK
	ELSE
	fdb	DUMP
	ENDC
	RFCS
	ldd	#NSTBOT
	subr	u,d
	lsrd			DEPTH is in D
	beq	@ndump3		Data stack is empty
	pshs	u
	ldu	#NSTBOT		Pointing to the bottom of the data stack
	lda	#1
	sta	CVISSGN		Force a signed number conversion
@ndump1	pshs	b
	ldx	,--u
	jsr	CVNSTR
	ldx	#TBUFF
# Skip leading spaces.
@ndump2	lda	,x+
	cmpa	#SP
	beq	@ndump2
	leax	-1,x
	jsr	PUTS
	lda	#SP
	jsr	PUTCH
	puls	b
	decb
	bne	@ndump1
	puls	u
@ndump3	rts

QRYDUP	fcb	4		79-STANDARD (REQ184)
	fcc	'?DUP'
	fdb	DDUMP
	RFCS
	jsr	NPOP
	UCNPUSH			Push back the original parameter
	cmpr	0,x
	bne	@qrydp1
	rts
@qrydp1	jmp	NPUSH		And DUP if NZ

TUCK	fcb	4		ANSI (Core ext)
	fcc	'TUCK'		( x1 x2 -- x2 x1 x2 ) i.e. SWAP OVER
	fdb	QRYDUP
	RFCS
	MINDREQ	2
	ldq	,u		D:W is X2:X1
	exg	d,w
	stq	,u
	tfr	w,x		X has X2
	jmp	NPUSH

NIP	fcb	3		ANSI (Core ext)
	fcc	'NIP'		( x1 x2 -- x2 ) i.e. SWAP DROP
	fdb	TUCK
	RFCS
	MINDREQ	2
	ldd	,u
	leau	2,u
	std	,u
	rts

DUP	fcb	3		79-STANDARD (REQ205)
	fcc	'DUP'
	fdb	NIP
	RFCS
	MINDREQ	1
	ldx	,u
	jmp	NPUSH

DROP	fcb	4		79-STANDARD (REQ233)
	fcc	'DROP'
	fdb	DUP
	RFCS
	jmp	NPOP

SWAP	fcb	4		79-STANDARD (REQ230)
	fcc	'SWAP'
	fdb	DROP
	RFCS
	MINDREQ	2		We need at least two cells stacked up
	ldq	,u		In place SWAP
	exg	d,w
	stq	,u
	rts

PICK	fcb	4
	fcc	'PICK'
	fdb	SWAP
	RFCS
	jsr	NPOP
PICK1	ldd	#NSTBOT
	subr	u,d
	lsrd			D has the data stack depth in cells
	cmpr	x,d
	bcc	@pick1
ERRPCK	ldb	#13		Argument is greater than DEPTH
	jsr	ERRHDLR		No return
@pick1	tfr	x,d
	tstd
	beq	ERRPCK
	decd			Minus 1, unlike in the Z80 implementation
	lsld			Times 2
	tfr	u,x
	leax	d,x
	tfr	x,y		For the sake of ROLL's implementation
	ldx	,x
	UCNPUSH
	rts

OVER	fcb	4
	fcc	'OVER'
	fdb	PICK
	RFCS
	ldx	#2
	bra	PICK1

ROLL	fcb	4
	fcc	'ROLL'
	fdb	OVER
	RFCS
	jsr	NPOP
ROLL1	tfr	x,w
	bsr	PICK1		Let PICK do the error handling
	leay	1,y		Point to the LSB since we're moving backward
	tfr	y,x
	leax	-2,x
	addr	w,w
	tfm	x-,y-
	jmp	NPOP

ROT	fcb	3
	fcc	'ROT'
	fdb	ROLL
	RFCS
	ldx	#3
	bra	ROLL1

MROT	fcb	4
	fcc	'-ROT'
	fdb	ROT
	RFCS
	RFXT	bsr,ROT+6	XT for ROT
	RFXT	bra,ROT+6	XT for ROT

CCOMMA	fcb	2		79-STANDARD (REF)
	fcc	'C,'
	fdb	MROT
	RFCS
	jsr	NPOP
	tfr	x,d
	ldy	DICEND
	stb	,y+
	sty	DICEND
	rts

COMMA	fcb	1		79-STANDARD (REQ143)
	fcc	','
	fdb	CCOMMA
	RFCS
	jsr	NPOP
	ldy	DICEND
	stx	,y++
	sty	DICEND
	rts

ALLOT	fcb	5		79-STANDARD (REQ154)
	fcc	'ALLOT'		( signedbytecount -- )
	fdb	COMMA
	RFCS
	jsr	NPOP
	ldy	DICEND
	addr	x,y
	sty	DICEND
	rts

FILL	fcb	4		79-STANDARD (REQ234)
	fcc	'FILL'
	fdb	ALLOT
	RFCS
	jsr	NPOP
	tfr	x,w		Byte fill value to W
FILL1	jsr	NPOP
	tfr	x,y		Byte count to Y
	exg	y,w		Byte count to W, byte fill value to Y
	jsr	NPOP		Destination address to X
	tstw
	ble	@filend
	tfr	y,d		Byte fill value to B
	stb	,x
	decw
	leay	1,x
	tfm	x+,y+
@filend	rts

BLANKS	fcb	6		79-STANDARD (REF152)
	fcc	'BLANKS'
	fdb	FILL
	RFCS
	ldw	#SP
	bra	FILL1

CMOVED	fcb	6		FORTH-83
	fcc	'CMOVE>'
	fdb	BLANKS
	RFCS
	jsr	ACQMOVP
	tstw
	beq	@cmovd1
	decw
	addr	w,x
	addr	w,y
	incw
	tfm	x-,y-
@cmovd1	rts

DCMOVE	fcb	6		79-STANDARD (REF)
	fcc	'<CMOVE'
	fdb	CMOVED
	RFCS
	jsr	ACQMOVP
	tfm	x-,y-
	rts

CMOVE	fcb	5		79-STANDARD (REQ153)
	fcc	'CMOVE'
	fdb	DCMOVE
	RFCS
	jsr	ACQMOVP
	tfm	x+,y+
	rts

MOVE	fcb	4		79-STANDARD (REQ113)
	fcc	'MOVE'		( srcaddr dstaddr ncells -- )
	fdb	CMOVE
	RFCS
	jsr	ACQMOVP
	tstw
	ble	@move1
	addr	w,w		Convert cells to bytes
	tfm	x+,y+
@move1	rts

CELLS	fcb	5		ANSI-X3.215-1994
	fcc	'CELLS'
	fdb	MOVE
	RFCS
	jsr	NPOP
	addr	x,x
	UCNPUSH
	rts

LAST	fcb	4		79-STANDARD (REF)
	fcc	'LAST'
	fdb	CELLS
	RFCS
	ldx	LSTWAD
	jmp	NPUSH

HERE	fcb	4		79-STANDARD (REQ188)
	fcc	'HERE'
	fdb	LAST
	RFCS
	ldx	DICEND
	jmp	NPUSH

PLUSST	fcb	2		79-STANDARD (REQ157)
	fcc	'+!'		( incr addr -- )
	fdb	HERE
	RFCS
	MINDREQ	2		We need at least two cells stacked up
	ldx	,u		ADDR to X
	ldd	,x		@ADDR to D
	addd	2,u		Add INCR to D
PLUSST1	std	,x		Store the sum back to ADDR
	leau	4,u		Drop two cells from the data stack
	rts

ONEPST	fcb	3		79-STANDARD (REF)
	fcc	'1+!'
	fdb	PLUSST
	RFCS
	MINDREQ	1
	ldx	,u
	ldd	,x
	incd
	std	,x
	leau	2,u
	rts

MINUSST	fcb	2		79-STANDARD (REQ157)
	fcc	'-!'		( incr addr -- )
	fdb	ONEPST
	RFCS
	MINDREQ	2		We need at least two cells stacked up
	ldx	,u		ADDR to X
	ldd	,x		@ADDR to D
	subd	2,u		Substract INCR from D
	bra	PLUSST1

CSTORE	fcb	2		79-STANDARD (REQ219)
	fcc	'C!'		( val8 addr -- )
	fdb	MINUSST
	RFCS
	MINDREQ	2		We need at least two cells stacked up
	lda	3,u		VAL8 to A
	sta	[,u]		Actual store to ADDR
	leau	4,u		Drop two cells from the data stack
	rts

STORE	fcb	1		79-STANDARD (REQ112)
	fcc	'!'		( data addr -- )
	fdb	CSTORE
	RFCS
	MINDREQ	2		At least two cells need to be stacked up
	ldd	2,u		DATA to D
	std	[,u]		Actual store to ADDR
	leau	4,u		Drop two cells from the user stack
	rts

CFETCH	fcb	2		79-STANDARD (REQ156)
	fcc	'C@'		( addr -- val8 )
	fdb	STORE
	RFCS
	MINDREQ	1		We need at least one cell stacked up
	clra
	ldb	[,u]
	std	,u
	rts

QMARK	fcb	1		79-STANDARD (REQ194)
	fcc	'?'
	fdb	CFETCH
	RFCS
	jsr	RAMFTCH		Call the RAM based incarnation of @
	RFXT	jmp,DOT+4	XT for .

* End of ROM part of the builtin dictionary.
THEEND	equ	*		This is the end, Beautiful friend
*				This is the end, My only friend

* This transactional word is relocated to RAM, so that we can compile new
* definitions. FORTHIN will take care of that and adjust the relevant pointers.
FETCH	fcb	1		79-STANDARD (REQ199)
	fcc	'@'		( addr -- data )
	fdb	QMARK
	RFCS
	MINDREQ	1		At least one cell needs to be stacked up
	ldd	[,u]
	std	,u		Returned through the data stack
	rts

REALEND	equ	*

*******************************************************************************
* String literals.

* Using CR+LF as it is Minicom's default.
BOOTMSG	fcb	FF		Form Feed (clear the screen in console context)
	fcc	'Z79Forth - 6309 FORTH-79 Standard Sub-set.'
	fcb	CR,LF
	fcc	'20200929 Copyright Francois Laagel (2020).'
	fcb	CR,LF,CR,LF,NUL

RAMOKM	fcc	'RAM0 check OK: 32 KB.'
CRLFSTR	fcb     CR,LF,NUL

RAMFM	fcc	'RAM0 check failure.'
	fcb     CR,LF,NUL

OKPRMPT	fcc	' OK'
	fcb	CR,LF,NUL

* Error messages for IODZHDL.
IOPERRM	fcn	'Illegal opcode near '
DV0ERRM	fcn	'Division by zero near '

ERRMTBL	fcn	'Data stack overflow'	Error 0
	fcn	'Data stack underflow'	Error 1
	fcn	'Undefined'		Error 2
	fcn	'User ABORT'		Error 3
	fcn	''			Error 4 (formerly "Division by zero")
	fcn	'Missing word name'	Error 5
	fcn	'Incorrect STATE'	Error 6
	fcn	'Return stack overflow'	Error 7
	fcn	'Return stack underflow' Error 8
	fcn	'Illegal construct'	Error 9
	fcn	'Assertion failed'	Error 10
	fcn	'R/O word'		Error 11
	fcn	'Missing delimiter'	Error 12
	fcn	'Illegal argument'	Error 13
	fcn	'No matching CREATE'	Error 14
	fcn	'Invalid BASE'		Error 15
	fcn	'Word name too long'	Error 16
	fcn	'IO error'		Error 17

AVL	equ	*		Marker of available EEPROM space left

*******************************************************************************
* Interrupt vector table
	org	VECTBL

	fdb	IODZHDL		Illegal opcode/Division by zero trap
	fdb	SWI3HDL		SWI 3 interrupt vector address
	fdb	SWI2HDL		SWI 2 interrupt vector address
	fdb	FIRQHDL		FIRQ interrupt vector address
	fdb	IRQHDL		IRA interrupt vector address
	fdb	SWIHDL		SWI interrupt vector address
	fdb	NMIHDL		NMI vector address
	fdb	RSTHDL		RESET vector address


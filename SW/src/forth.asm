* 6309 Forth based on my 1984/07/16 TRS-80 Model 1 level II ROM implementation.
*
* This work is dedicated to Alain Pinaud, author of "Programmer en Forth"
* published by Editions du P.S.I. in 1983--i.e. my virtual mentor in the field.
* Special thanks go William Astle for his fantastic LW Tools utility set and
* to Robert Lipe for his advice on serial line handling. Apache subversion
* and the sdiff utility also were on my side all the time.
*
* Also credited for their help: Justin Poirier (seminal HW design), Daniel
* Tufvesson (original CompactFlash interface), Peter Minuth (general Forth
* guruness), Paul E. Bennett (ANSI compatibility advice); Michel Jean, Bill
* Ragsdale, Pablo Hugo Reda, Demitri Peynado and Bernd Paysan for contributed
* application level code; Carsten Strotmann for most of the benchmarking code
* (see https://theultimatebenchmark.org/); Gerry Jackson and Steve R. Palmer
* (see https://github.com/gerryjackson/forth2012-test-suite) for selected
* bits and pieces of the Forth2012 test suite (see https://forth-standard.org/).
*
* This is a native Forth. Not a threaded interpretive implementation.
* Worth noticing is the fact that the return stack does not hold return
* addresses at all. All what is stored there is loop indexes and input stream
* parameters. Actual return addresses are kept in the system stack. All in all,
* this is the result of the work of a 19 year old, heavily revisited 35 years
* later. The downside of this implementation is that the generated code is
* about 30% larger than a threaded interpretive implementation would be.
* So it goes...
*
* Control flow constructs have been re-implemented based on Wonyong Koh's
* hForth for the 8086. The original code can be consulted at
* https://github.com/nealcrook/hForth. Of particular interest is
* 8086/HF86RAM.ASM. Entries on the control flow stack (implemented on the
* data stack here) are two cell entities that consist of a parameter
* (an address), on the top of which a tag identifying the type of address
* is pushed. They are:
*
* Control-flow stack item    Representation (parameter and type)
* -----------------------    -------------------------------------
* dest                       control-flow destination      0
* orig                       control-flow origin           1
* of-sys                     OF origin                     2
* case-sys                   x (any value)                 3
* do-sys                     ?DO origin                    DO destination
* colon-sys                  xt of current definition     -1 [1]
*
* [1] Not implemented in Z79Forth/A.
* 
* This mechanism allows for verification of balanced control flow constructs.
*
* The code generated is limited to a very small instruction set:
*
* LDXOPC	$8E	LDX (immediate)
* JMPOPC	$7E	JMP (extended)
* JSROPC	$BD	JSR (extended)
* RTSOPC	$39	RTS (inherent)
* BCSOPC	$2503	BCS *+5	(relative) Used in LOOP, +LOOP
* BNEOPC	$2603	BNE *+5	(relative) Used in ?DO, IF, UNTIL
*
* On error, the system stack pointer is reset. The return stack also is
* but the data stack will be in the same state as when the error occurred.
* ABORT and QUIT enforce their own ANS94 standard behaviour.
*
* RESTRICT is non-standard. It comes from GNU Forth (VolksForth). The " OK"
* non-prompt string also does, by the way. Thanks to Anton Ertl for his terse
* yet valuable input.
*
* .' (dot-tick) is non-standard. It comes from SwiftForth. It will be supported
* whether or not the symbolic stack dump has been configured (see SSDFEAT in
* constants.asm). However, please note that effective symbolic references will
* be resolved only if the feature was enabled at compilation time. Otherwise
* a simple HEX print of the cell at the top of the data stack will be
* performed. This is meant to support Forth source code that does not depend
* on the feature vector (see examples/dis.4th).
*
* MONITOR and ICHECK are also non-standard. They are intended to maintain and
* verify the integrity of a checksum of the code section of RAM resident
* words, with the exception of VARIABLEs and CREATEd words. CREATEd words,
* if subject to the MONITOR treatment will also carry a code section
* checksum, although this is not the default behaviour. The checksum will
* consist in an extra byte added to every word's header. MONITOR and ICHECK
* will be available whether or not the reliability feature has been configured
* (see RELFEAT in constants.asm). They will only carry effective semantics
* if the feature was actually enabled at compilation time. This is meant to
* support Forth source code that does not depend on the feature vector.
*
* Experimental MC146818 RTC support: the feature is disabled by default
* (see RTCFEAT in constants.asm) because it relies on undocumented schematics.
* Also, the required underlying circuitry is not intended to ever become an
* integral part of the Z79Forth reference board itself. Some better designed
* form of it might eventually surface in the form of an extension specification.
* At the time of this writing this is just a proof of concept, but whether or
* not the feature is enabled, three extra words will be added to the dictionary.
* They are RTC@ RTC! and TICKS They can be safely ignored.
*
* Forth source code portability note:
* Because Z79Forth does not use the return stack to store return addresses,
* it makes it quite easy to write code that is not readily portable to classic
* threaded interpretive implementations. The resulting code will be simpler but
* portability will be limited.
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
* written it myself." John E. Littlewood (1885-1977).
*
* The original soundtrack for this work is available at:
* https://youtu.be/YqXZtGyFyDo?t=4023 (J.S. Bach BWV 1080, contrapunctus 14).

pragma	opt cd,operandsizewarning

	include	constants.asm

* Unchecked NPUSH. This is for situations in which there is absolutely no
* chance of overflow. For instance, in case we just popped 2 cells and push
* back one or two.
UCNPUSH	MACRO	NOEXPAND
	pshu	x
	ENDM

* Unchecked NPOP. To be used only after trusted words have been invoked, i.e.
* when one is positively sure that the data stack contains at least one cell.
UCNPOP	MACRO	NOEXPAND
	pulu	x
	ENDM

* Make sure minimum data stack requirements are satisfied upon word entry.
* The requirement is expressed in cell size--2 bytes on the 6309.
* \1 specifies the required number of cells.
MINDREQ	MACRO	NOEXPAND
	ldd	#NSTBOT-(\1*2)	Cell count one expects to be stacked up
	bra	CHKNDPT		No return if the condition is not met
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
	ENDC			RELFEAT
	ENDM

* Reliability feature support: variable word header contents.
* This is a dummy checksum only used for EEPROM resident words.
* Those never are checked for code integrity by MONITOR.
RFCS	MACRO	NOEXPAND
	IFNE	RELFEAT
	fcb	ILLOPC		Illegal opcode
	ENDC			RELFEAT
	ENDM

*******************************************************************************
* RAM definitions. We cannot initialize globals from the assembly defs.
* All we can do here is define addresses and size things up.

* RAMSTART is 0. We leave page 0 unused as a bug proof area.
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
PLOAD	rmb	2		Word payload if found by SWDIC
FNDPLD	rmb	2		Last code payload reported by FIND
RECADDR	rmb	2		Used by RECURSE
JSRLAST	rmb	2		Last compilation address of #JSROPC
FWDREF	rmb	2		Address of the last forward reference
RAKEVAR	rmb	2		Linked list of LEAVE forward references
VLPRVEP	rmb	2		Used in WORDS to compute word code length
VLPRVHD	rmb	2		Used in WORDS to compute word code length
MRUBUFA	rmb	2		Most recently used buffer address
BSBFADR	rmb	2		Base buffer address for the input stream

* Global variables.
UBASE	rmb	2		Base for numbers input and output--BASE
USTATE	rmb	2		0 if interpreting, 255 if compiling--STATE
ISLEN	rmb	2		Input stream length
ISEADDR	rmb	2		End of input stream address (included)
UTOIN	rmb	2		User variable for >IN
UBLK	rmb	2		User variable for BLK
USCR	rmb	2		User variable for SCR (output for LIST)
TIKSHI	rmb	2		RTC clock ticks updated on FIRQ
TIKSLOW	rmb	2		RTC clock ticks updated on FIRQ
	IFNE	DEBUG
CCREG	rmb	2		A DEBUG variable for predicates (see CMP2)
	ENDC			DEBUG
	IFNE	HVNMI
	IFNE	HVNMI2
SBDROPC	rmb	2		Char. drop count for serial input (see FIRQHDL)
	ENDC			HVNMI2
	ENDC			HVNMI
ANCMPF	rmb	1		Anonymous compilation flag
BALNCD	rmb	1		Balanced flag for control flow constructs
BASBKUP	rmb	1		BASE backup when a base prefix is in use
RDEPTH	rmb	1		Return stack depth in cells
DIVFCN	rmb	1		Flag used by /, MOD and /MOD
DIVDBL	rmb	1		DIV: N1 is a double (flag)
DIVSYM	rmb	1		DIV: symmetric division required (flag)
MULFCN	rmb	1		Flag used by *, M*
F83DIVF	rmb	1		FORTH-83 adjusment flag for floored division
STSLFCN	rmb	1		Flag used by */, */MOD
CVTFCN	rmb	1		CVT: 0 => # semantics, 1 => #S semantics
ISNEGF	rmb	1		Number being scanned is negative
ISDBLF	rmb	1		Number being scanned is a double
CVISSGN	rmb	1		Flag: should CVNSTR consider numbers as signed
CURTOKL	rmb	1		Current token length. Set by SWDIC
IMDFLG	rmb	1		Immediate flag
DEFFLG	rmb	1		Define flag
RTCAVL	rmb	1		NZ if real time clock is present
CFCARDP	rmb	1		NZ if CF card present
CFCMMIR	rmb	1		Last CF command issued
CFERRCD	rmb	1		and the corresponding error code
SRCID	rmb	1		ANSI SOURCE-ID (internal only).

* Serial buffer parameters. Queing happens on FIRQ.
* Dequeing occurs when GETCH is invoked.
SERBENQ	rmb	1		Enqueue offset
SERBDEQ	rmb	1		Dequeue offset
SERBCNT	rmb	1		Buffer byte count
XMITOK	rmb	1		Software flow control on output flag
SERBUF	rmb	SERBSZ		The actual buffer

PADBUF	rmb	PADBSZ		PAD lives here.
APADBUF	rmb	PADBSZ		Alternate PAD here. Used by <#, #, #S, #>

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

TBUFF	rmb	TBUFSZ		Output for CVNSTR

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
* might still be overridden by user definitions. WORDS will happily ignore
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
	ldy	#HEXBUF
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
IRQHDL	equ	*
SWIHDL	equ	*
	IFEQ	HVNMI
NMIHDL				These should never happen
	ENDC
	rti

* Interrupts are disabled by default upon reset.
* NMI# will not be "armed" until S is initiliazed.
RSTHDL	ldmd	#1		Establish 6309 native mode

	lda	#ACIRSET
	sta	ACIACTL		ACIA master reset
	lda	#ACIRTS1
	sta	ACIACTL		RTS# high, 8N1

	ldx	#BOOTMSG	Identity statement

* Send the NUL terminated string pointed to by X to the ACIA.
* The RAM is not yet assumed to be working.
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
	ENDC			DEBUG
	tfm	x+,y+

* Initialize the system stack pointer and the direct page base address register.
	lds	#RAMSTRT+RAMSIZE
	lda	#VARSPC/256
	tfr	a,dp
	SETDP	VARSPC/256

* Serial buffer parameters initialization. We are doing this here because
* PUTS requires prior software flow control initialization.
	IFNE	DEBUG
	clrd
	std	SERBENQ		Two birds with one stone
	sta	SERBCNT
	IFNE	HVNMI
	IFNE	HVNMI2
	std	SBDROPC		Initialize chararacter drop count
	ENDC			HVNMI2
	ENDC			HVNMI
	ENDC			DEBUG
	lda	#1		Initialize software flow control on output
	sta	XMITOK

	ldx	#RAMOKM
	jsr	PUTS
	jsr	FORTHIN		Global variables initialization
	jsr	CFINIT		CompactFlash card initialization
	IFNE	RTCFEAT
	jsr	RTCINIT		Real time clock initialization
	ENDC			RTCFEAT

* Lower RTS and enable FIRQ.
	lda	#ACIRTS0
	sta	ACIACTL
	andcc	#^FFLAG

	tst	CFCARDP
	beq	INTERP

* A CF card is present, LOAD block #1.
	ldx	#1
	jsr	LOAD1

* The interpreter main loop.
INTERP	clrd
	std	UBLK		Interpreting from the console. Set BLK to 0
	std	UTOIN
	sta	SRCID		Clear SOURCE-ID. Not invoked in EVALUATE context
	ldx	#CMDBUF
	stx	BSBFADR
	ldb	#CMDBFSZ	No NUL terminator in this implementation
	jsr	GETS		Acquire command from the console

* Additional setup in case the ANSI \ is used.
	clra
	std	ISLEN		GETS returns the entered character count via B

	clrb
	std	ISEADDR		Clear end of input stream address (included)
	bsr	_INTERP
MINTLRA	bra	INTERP

* The interpreter itself.
_INTERP	jsr	SCNSTOK		Scan for the beginning of a word at address X
	beq	@oeistr		This is the end
	tfr	x,d		Starting token address to D
	jsr	U2INFRD		Derive >IN from D
	tst	USTATE+1	We do ignore the upper byte
	bne	COMP		We are compiling
	jsr	SWDIC		Updates TOKENEP, CURTOKL, IMDFLG/DEFFLG
	bne	@exec		Word found, execute it
	jsr	NUMCVT
NMCVIRA	equ	*
	ldx	TOKENEP
	bra	_INTERP		Next token, please!
* End of input stream condition is recognized. We are looking at the past here.
@oeistr	tst	SRCID		Were we running a string via EVALUATE?
	bne	@done		No feedback if that was the case
	ldd	UBLK
	beq	@feedbk		We are back from the console
@done	rts			We're done here
@feedbk	ldx	#OKFEEDB	Provide OK feedback
	tst	USTATE+1	No OK feedback if we're compiling, just CRLF
	beq	@fullfb
	leax	3,x		Skip the ' OK' string when compiling
@fullfb	jmp	PUTS		Back to whoever invoked us
@exec	lda	DEFFLG
	beq	@introk		Compilation only flag is not set
	ldb	#6		Incorrect STATE
	jsr	ERRHDLR		No return
INTISRA	equ	*		For symbolic stack debugging purposes
@introk	ldd	TOKENEP
	bsr	U2INFRD		Derive >IN from D
	ldx	#INTRPRA	The return address
	pshs	x
	tfr	y,pc		An indirect call to Y
INTRPRA	jsr	BKIN2PT		Derive X from BLK, >IN
	bra	_INTERP

* The compiler.
* Upon entry TOKENSP has been set by a prior call to SCNSTOK.
COMP	jsr	SWDIC		Updates TOKENEP, CURTOKL, IMDFLG/DEFFLG
	beq	@cmpnum		Word @ TOKENSP is not in the dictionary
	tst	IMDFLG
	beq	@notimd
	ldd	TOKENEP
	bsr	U2INFRD		Derive >IN from D
	ldx	#COMPLRA	Word is immediate. Execute it.
	pshs	x		Return to COMPLRA
	tfr	y,pc		An indirect call to Y
COMPLRA	jsr	BKIN2PT		Derive X from BLK, >IN
	stx	TOKENEP
	bra	_INTERP		Branch back to the interpreter
@notimd	tfr	y,x
	bsr	EMXASXT		Emit X as an execution token
@cmpdon	ldx	TOKENEP
	bra	_INTERP
@cmpnum	jsr	NUMCVT
NMCVCRA	equ	*
	tst	ISDBLF
	bne	@cmpdbl
	UCNPOP			TOS to X
	jsr	LITER
	bra	@cmpdon
* The following is some sort of half baked 2LITERAL.
* Please note that NUMCVT guarantees us that at least two cells are stacked up.
@cmpdbl	ldx	2,u
	jsr	LITER		LSC
	ldx	,u
	jsr	LITER		MSC
	leau	4,u		2DROP
	bra	@cmpdon

* Emit (in a code generation understanding) X as an execution token.
* In essence, this simply inserts JSR <X> at HERE.
* Note: this code provides support for trailing JSR elimination.
* On input: X has the target execution token.
* On output: Y will have HERE, A will be altered, X will be preserved.
EMXASXT	ldy     DICEND
	sty	JSRLAST		JSRLAST points to the latest JSR code emission
	lda	#JSROPC		JSR extended
	jsr	VARCON2		Compile a JSR to the execution token
	sty	DICEND
	rts

* Derive UTOIN from D's current value. D is altered.
U2INFRD	subd	BSBFADR
	std	UTOIN
	rts

* Store the HEX representation of the lower nibble of A to Y+.
HEX1D	pshs	b
	ldb	#'0
	anda	#$0f
	cmpa	#10
	bcs	@hex1d1
	ldb	#'A-10
@hex1d1	addr	b,a
	sta	,y+
	puls	b
	rts

ADIV16	lsra
	lsra
	lsra
	lsra
	rts

* Hexdump D to 4 bytes starting at Y.  Upon return, Y will point 1 byte
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
	clr	,y
	rts

HDMP2	pshs	d
	bsr	ADIV16
	bsr	HEX1D
	lda	,s
	bsr	HEX1D
	puls	d
	clr	,y
	rts

	IFNE	HVNMI
* Add string pointed to by X starting at the address stored in Y.
ADDS	pshs	a
@adds1	lda	,x+
	sta	,y+
	bne	@adds1
	puls	a
	rts

NMI2DM	bsr	ADDS
	leay	-1,y		Backward over NUL
	bra	HDMP2

NMI4DM	bsr	ADDS
	leay	-1,y		Backward over NUL
	bra	HDMP4

NMIDML	ldx	#TBUFF
	jsr	PUTS
	jmp	PUTCR

* All registers are stacked in native mode.
NMIHDL	lda	ACIACTL
	pshs	a
	lda	XMITOK
	pshs	a
	lda	#1
	sta	XMITOK
* Stack structure at this point
* 0	saved XMITOK: 1 byte
* 1	saved ACIA status register: 1 byte
* 2	CC
* 3	D
* 5	W
* 7	DP
* 8	X
* 10	Y
* 12	U
* 14	PC
	jsr	PUTCR

* First line: CC, D, W, DP, X, Y, U, PC, S
	ldy	#TBUFF
	ldx	#CCREGM
	lda	2,s		CC in the system stack
	bsr	NMI2DM

	ldd	3,s		D in the system stack
	bsr	NMI4DM

	ldd	5,s		W in the system stack
	bsr	NMI4DM

	lda	7,s		DP in the system stack
	bsr	NMI2DM

	ldd	8,s		X in the system stack
	bsr	NMI4DM

	ldd	10,s		Y in the system stack
	bsr	NMI4DM

	ldd	12,s		U in the system stack
	bsr	NMI4DM

	ldd	14,s		PC in the system stack
	bsr	NMI4DM

	leau	16,s		S in the system stack
	tfr	u,d
	bsr	NMI4DM

	bsr	NMIDML

	IFNE	HVNMI2
* Second line: ACIST, XMTOK, SBASE, SBENQ, SBDEQ, SBCNT, SBDROPC
	ldy	#TBUFF
	ldx	#ACISTM
	lda	1,s		ACIA status register in the system stack
	bsr	NMI2DM

	lda	,s		XMITOK in the system stack
	bsr	NMI2DM

	ldd	#SERBUF		SERBUF address
	bsr	NMI4DM

	lda	SERBENQ		SERBENQ 8 bit offset
	bsr	NMI2DM

	lda	SERBDEQ		SERBDEQ 8 bit offset
	bsr	NMI2DM

	lda	SERBCNT
	bsr	NMI2DM

	ldd	SBDROPC		Number of bytes dropped because SERBUF was full
	bsr	NMI4DM

	bsr	NMIDML
	ENDC			HVNMI2

	leas	2,s		System stack cleanup
	rti

CCREGM	fcn	'CC '
DREGM	fcn	' D '
WREGM	fcn	' W '
DPREGM	fcn	' DP '
XREGM	fcn	' X '
YREGM	fcn	' Y '
UREGM	fcn	' U '
PCREGM	fcn	' PC '
SREGM	fcn	' S '
	IFNE	HVNMI2
ACISTM	fcn	'AS '		ACIA status register
XMTOKM	fcn	' XO '		XMITOK--software flow control (one byte)
SBASEM	fcn	' SB '		Serial FIFO base address (two bytes)
SBENQM	fcn	' EN '		FIFO enqueue offset (one byte)
SBSEQM	fcn	' DE '		FIFO dequeue offset (one byte)
SBCNTM	fcn	' CN '		FIFO queued byte count (one byte)
SBDRPM	fcn	' DR '		Number of characters dropped (two bytes)
	ENDC			HVNMI2

	ENDC			HVNMI

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

FORTHIN	RFXT	jsr,NCLR+7	XT for NCLR. Set up the normal stack
	RFXT	jsr,RCLR+7	XT for RCLR. Set up the return stack
* Relocate '@' code to RAM and set it up as the last dictionary entry (RO).
	ldx	#THEEND		Source address for tfm
	ldw	#(REALEND-THEEND) Byte count for tfm
	ldy	#WDICSPC	Destination address for tfm
	sty	LSTWAD
	tfm	x+,y+
	sty	DICEND
	IFNE	RELFEAT
	RFXT	jsr,MONITOR+10	XT for MONITOR (monitor @ in RAM)
	ENDC			RELFEAT
	jsr	EMPTYB		Buffer related initializations
	IFNE	DEBUG
	clrd
	std	USTATE		Initial mode is interpretation
	std	USCR		Clear SCR
	std	UBLK		Clear BLK
	std	UTOIN		Clear >IN
	sta	SRCID
	ENDC			DEBUG
	RFXT	jmp,DECIMAL+10	XT for DECIMAL. Default base is decimal

* Input stream end reached? If ISEADDR is clear, compute it, then compare
* X to it. EOL condition should be handled if we return from this with ZFLAG
* set. BSBFADR and ISLEN must have been previously initilized.
ISERCHD	pshs	d
	ldd	ISEADDR
	beq	@iseset
@cont	cmpr	x,d
	puls	d
	rts
@iseset	ldd	BSBFADR
	addd	ISLEN
	std	ISEADDR
	bra	@cont

* Scan for the next non-space character pointed to by X.
* Return with ZFLAG set if an end of input stream condition is recognized,
* otherwise ZFLAG will be clear. A is altered.
* Update TOKENSP in all cases.
SCNSTOK	bsr	ISERCHD		End of input stream reached?
	beq	@scstk1		Yes, update TOKENSP and return w. ZFLAG set
	lda	,x+
	cmpa	#SP
	beq	SCNSTOK
	leax	-1,x		Backward one character
* LEA affects ZFLAG but it will remain clear at this point.
@scstk1	stx	TOKENSP		This affects ZFLAG!!!
	tsta			Have to test again because LEA affects Z
	rts

* Scan for the next white space character as an end of token marker.
* Upon entry:
* - X points to the input stream.
* Upon return:
* - X will point to the next space character or one character after the
*   end of the input stream.
* - CURTOKL will hold the current token length (returned in B).
* - TOKENEP will point to the end of the current token.
* - A is altered.
* This routine assumes there was an identified start of token in the past,
* i.e. that X was not pointing to a BL character upon entry.
SCNETOK	clrb
@scetok	bsr	ISERCHD		End of input stream reached?
	beq	@scetk1
	lda	,x+
	incb
	cmpa	#SP
	bne	@scetok
	leax	-1,x		Keep pointing at the trailing space
	decb			Uncount BL if that was the end of token marker
@scetk1 tfr	cc,a
	stx	TOKENEP
	stb	CURTOKL
	tfr	a,cc
	rts

NXTCHAR	bsr	ISERCHD
	beq	@gtnxc1		End of input stream detected. Return with Z set
	lda	,x+
@gtnxc1	rts

* Check for numeric literal BASE prefix. On entry X has the input stream
* pointer. On exit, BASE is altered if needed and the original BASE saved
* to BASBKUP. If BASE was not changed, BASBKUP will be zero.
* D is altered, X is updated if a BASE prefix is detected, other registers
* are untouched.
CKNBPFX	ldb	,x		B has a potential base prefix character
	pshs	x
	ldx	#BASALST	A associative list (A-list) of BASE prefixes
@pflkup	lda	,x++		Potential BASE prefix character to A
	beq	@nopfix		Reached the end of the A-list. No prefix found
	cmpr	b,a		Prefix match?
	beq	@pfxfnd		Yes
	bra	@pflkup		Perform base prefix lookup
@nopfix	clr	BASBKUP		Nothing to be restored to BASE
	puls	x
	rts
@pfxfnd	lda	UBASE+1
	sta	BASBKUP		Back up the current BASE value
	lda	-1,x		The BASE specified by the prefix
	sta	UBASE+1		Update BASE
	puls	x
	leax	1,x		Skip the prefix from the input stream
	rts

* Restore BASE if a numeric literal BASE prefix was detected.
* X is to be preserved at all cost!
RSBSPFX	lda	BASBKUP
	beq	@theend
	sta	UBASE+1
@theend	rts

* Check whether BASE is in the supported range ([2..36]).
CKBASE	lda	UBASE+1		BASE
	cmpa	#2
	blo	@ckbser		Must be >= 2
	cmpa	#36
	bhi	@ckbser		And <= 36 (the ANSI maximum)
	rts
@ckbser	ldb	#15		Invalid BASE
	jsr	ERRHDLR		No return
CKBASRA	equ	*

* NUMCVT performs a signed string to number conversion. The input string is
* acquired from the input stream. Note that this routine always is called
* after a dictionary lookup (SWDIC), so TOKENSP and CURTOKL are guaranteed
* to have been set previously. There are two ways out this routine:
* - redirection to the error handler (Undefined word), or
* - a converted cell or double cell returned through the data stack.
*   Upon return, ISDBLF being NZ will indicate a double.
NUMCVT	bsr	CKBASE		No return if BASE isn't in the [2..36] range
	clr	ISNEGF		Assume the result is positive
	clr	ISDBLF		Assume the result is not a double number
	ldx	TOKENSP
	bsr	CKNBPFX		Check for numeric literal BASE prefix
* Check for optional minus sign.
	lda	,x
	cmpa	#'-
	bne	@ncini
	inc	ISNEGF		Remember to negate the result before returning
	leax	1,x		Skip the negativity!
@ncini	tfr	x,y		Backup input stream pointer
	tfr	0,x		Initialize the result
	jsr	NPUSH		LSC
	jsr	NPUSH		MSC
	tfr	y,x		Restore the input stream pointer
	leax	-1,x
	jsr	NPUSH		Base scanning address minus 1
	RFXT	jsr,CONVERT+10
* Upon return TOS C@ should be BL or . Anything else indicates an error.
* In any case, at this point, at least three cells are on the data stack.
	UCNPOP			Address of the last non convertible char to X
	jsr	ISERCHD
	beq	@ncadj		End of input stream detected
	lda	,x
	cmpa	#SP
	beq	@ncadj		BL is acceptable
	cmpa	#'.		Was a double number meant?
	bne	@ncnogo		No, we do not have a winner...
	inc	ISDBLF
* If Z is not set at this point, we are dealing with a double number.
@ncadj	bne	@ncdadj	
	UCNPOP			Drop the MSC
	tst	ISNEGF		Are we dealing with a negative number?
	beq	RSBSPFX		No. Restore BASE if needed--the end
	RFXT	jsr,NEGATE+9	Acknowledge the negativity
	bra	RSBSPFX		Restore BASE if needed--the end
@ncdadj	tst	ISNEGF		Are we dealing with a negative number?
	beq	RSBSPFX		No. Restore BASE if needed--the end
	RFXT	jsr,DNEG+10	Acknowledge the negativity
	bra	RSBSPFX		Restore BASE if needed--the end
@ncnogo	leau	4,u		Drop two cells from the data stack
	ldx	TOKENSP		Beginning address of the current token
	ldb	#2		Undefined (X points to the offending word)
	jsr	ERRHDLR		No return
NUMCVRA	equ	*		For symbolic stack dump purposes
	nop

* Convert number stored in X to a string (depending on BASE value).
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
* BASE has letters among its valid numbers.
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

* Check for minimal data stack depth. On input D has the lowest possible stack
* address that satisfies the needs of the caller. This routine is meant
* to support "transactional" behaviour, which is intended to improve
* debugging support.
CHKNDPT	cmpr	d,u
	bhi	@stkudf
	rts
@stkudf	ldb	#1		Data stack underflow
	jsr	ERRHDLR		No return
CKDPTRA	equ	*

* Parameter stack's depth checking primitives (transactional behavior support).
MIN1PST	MINDREQ	1

MIN2PST	MINDREQ	2

MIN3PST	MINDREQ	3

MIN4PST	MINDREQ	4

* Search word beginning at address TOKENSP in the dictionary.
* Upon return Z will be set if the word was not found.
* Otherwise, Y will point to the code section of the word.
* CSSNTVE (defined in constants.asm) defines whether the
* dictionary match is case sensitive or not.
* Important note: if the word is found TOKENEP will be copied to TOKENSP.
SWDIC	ldx	TOKENSP
	jsr	SCNETOK		B has CURTOKL, update TOKENEP
SWDIC1	ldx	DICEND		Entry point for the ANSI FIND
	stx	VLPRVEP		Last dictionary entry code address + 1
	ldx	TOKENSP
	ldy	LSTWAD		Latest word header address to Y
@swrdc0	bne	@swrdc1
	tfr	0,y		Word not found. Z is set
	sty	PLOAD
	rts
@swrdc1	lda	,y		Word attribute to A
	anda	#WRLNMSK	Extract word length
	pshs	y
	cmpr	a,b		Word length match?
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
@nochg	cmpr	a,b
	tfr	e,b
	ENDC			CSSNTVE
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
	ENDC			RELFEAT
	ldd	VLPRVEP
	subr	y,d
	std	PLOAD
	rts			NZ since there is no zero payload word
@swrdc3	puls	y
	sty	VLPRVEP
	clra
	ldb	,y+
	andb	#WRLNMSK
	leay	d,y
	ldx	TOKENSP
	ldb	CURTOKL
	ldy	,y		Point to previous word header
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
@locwr0
	ENDC			DEBUG
	stx	BDICEND		Back pointer up
	ldx	LSTWAD
	stx	BLSTWAD		Back pointer up
	jsr	BKIN2PT		Derive X from BLK, >IN
	jsr	SCNSTOK		Locate token starting address
	bne	@locwr1
	ldb	#5		EOIS condition recognized: missing word name
	jsr	ERRHDLR		No return
LWMNRA	equ	*		LOCWRT missing word name return address
@locwr1	jsr	SCNETOK		X has TOKENEP, B has CURTOKL
	ldy	TOKENSP
	subr	y,x
	pshs	x		Word length to the system stack
	ldx	DICEND
	lda	1,s		Word length LSB in the system stack
	cmpa	#1+WRLNMSK	Max word length is 31, 79-STANDARD compliant
	blo	@locwr2
	ldb	#16		Word name is too long
	jsr	ERRHDLR		No return
WTOOLNG	equ	*
@locwr2	sta	,x+		Word length to dictionary
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
	ENDC			CSSNTVE
	tfr	x,d
	jsr	U2INFRD		Derive >IN from D
	ldx	LSTWAD
	stx	,y++		Back pointer to dictionary
	IFNE	RELFEAT
	clr	,y+		Initialize the checksum header field
	ENDC			RELFEAT
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
	sty	JSRLAST		JSRLAST points to the latest JSR code emission
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
CMP2	jsr	MIN2PST		At least 2 cells must be stacked up
CMP2RA	ldy	,u
	ldx	2,u
	cmpr	y,x
	tfr	cc,b
	IFNE	DEBUG
	clra
	std	CCREG
	ENDC			DEBUG
	tfr	0,x
	leau	4,u
	tfr	b,cc
	rts

* Used by CMOVE, CMOVE>, MOVE.
ACQMOVP	jsr	MIN3PST		At least 3 cells must be stacked up
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
	ENDC			RELFEAT
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
	tfm	x+,y+
	clr	,y		We need this in case the offset is zero
* Offset processing.
	IFNE	RELFEAT
	leax	3,x		Skip backlink and checksum
	ELSE
	leax	2,x		Skip backlink
	ENDC			RELFEAT
	ldd	2,s		Execution token to D
	subr	x,d		Offset between XT and word entry point to D
	beq	@skoffs		Skip displaying the offset if it is zero
	pshs	a		Preserve the offset's MSB
	lda	#'+
	sta	,y+
	puls	a		Restore the offset's MSB
	jsr	HDMP4		Dump hex incarnation of the offset to Y
@skoffs	puls	x,y
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
@dctmfn	leas	2,s		Drop X from the system stack
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
	fdb	CHKNDPT		Check data stack minimum depth (transac. behav.)
	fcn	'CHKNDPT'
	fdb	CMP2RA		Missing operand in any of U<, U>, <, >
	fcn	'CMP2RA'
	fdb	ACQVMRA		Three operands missing in any of CMOVE,
*				CMOVE>, MOVE
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
	fdb	COMPLRA		Compiler RA (after the execution of an IMD word)
	fcn	'COMPLRA'
	fdb	EMXASXT		Emit "JSR <X>" where X has an execution token
	fcn	'EMXASXT'
	fdb	LWMNRA		Missing word name in LOCWRT
	fcn	'LWMNRA'
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
	IFNE	DEBUG
	fdb	LWAFRA		Assertion failure in LOCWRT
	fcn	'LOCWRTAF'
	ENDC
	fdb	0		End of list
	fcn	'???'		Admit we have no clue!
	ENDC			SSDFEAT

* Print ' (xxxx/yyyy)' where xxxx is the hex representation for BLK @ and
* yyyy is the hex representation for >IN @. Y is preserved, X and D are not.
PRBLKIN	pshs	y
	ldy	#HEXBUF
	lda	#SP
	sta	,y+
	lda	#'(
	sta	,y+
	ldd	UBLK
	jsr	HDMP4
	lda	#'/
	sta	,y+
	ldd	UTOIN
	jsr	HDMP4
	lda	#')
	sta	,y+
	clr	,y
	ldx	#HEXBUF
	jsr	PUTS
	puls	y
	jmp	PUTCR

* Handle error condition. Error code is in B.
* If B is 2 (undefined) X points to a string of length CURTOKL that has the
* offending word.
ERRHDLR ldy	,s		Invoking return address
* In case of a trap return, we enter here with Y set to #IODZHDL
ERRHD1	jsr	PUTCR		GNU Forth does this in its exception handler
	cmpb	#2		Undefined symbol?
	bne	@perrm		No
	lda	#''		Begin quote
	jsr	PUTCH
@prtsym	lda	,x+		Display undefined symbol name
	jsr	PUTCH
	dec	CURTOKL
	bne	@prtsym
	lda	#''		End quote
	jsr	PUTCH
	lda	#SP		BL EMIT
	jsr	PUTCH
@perrm	ldx	#ERRMTBL	Regular error handling
@nxterr	tstb
	bne	@skerrm
	jsr	PUTS		Print error message
	bsr	PRBLKIN		Print BLK and >IN values (in hex)
@dmptos	tfr	y,d		Dump top of the system stack contents
	IFNE	SSDFEAT
	pshs	d
	ENDC			SSDFEAT
	ldy	#HEXBUF
	jsr	HDMP4
	lda	#SP
	sta	,y+
* Symbolic stack dumps are configurable. In situations where the dictionary is
* trashed, they may not be considered desirable. See SSDFEAT in constants.asm.
	IFNE	SSDFEAT
	tfr	y,x		X has the buffer pointer
	puls	y		Restore target symbol table entry
	jsr	FINDSYM
	ELSE
	clr	,y		No symbolic information is to be printed
	ENDC			SSDFEAT
	ldx	#HEXBUF
	jsr	PUTS
	jsr	PUTCR
	cmpy	#IODZHDL
	beq	@wastrp		We're just back from the trap handler
	leas	2,s		Point to the next item on the stack
@wastrp	cmps	#RAMSTRT+RAMSIZE
	bhs	@errdon		We're done here
	ldy	,s
	bra	@dmptos
@skerrm	lda	,x+		Scan for the next error message
	bne	@skerrm
	decb
	bra	@nxterr
@errdon	lds	#RAMSTRT+RAMSIZE
	tst	USTATE+1	We do ignore the upper byte
	beq	@erdon2		No pointers to restore if we were interpreting
* We were compiling: clear STATE; restore DICEND and LSTWAD, if not :NONAME.
	clr	USTATE+1	Switch back to interpretation mode
	ldx	BDICEND		Restore essential pointers from backups
	stx	DICEND		Restore HERE
	tst	ANCMPF
	bne	@clrano
	ldx	BLSTWAD
	stx	LSTWAD		Restore LAST
@clrano	clr	ANCMPF
@erdon2	RFXT	jsr,RCLR+7	Clear the return stack and
	RFXT	jsr,NCLR+7	the control flow stack (moved here from ABORT)
	RFXT	jsr,DECIMAL+10	Back to decimal BASE, for one's sanity sake!
	jmp	INTERP

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
	cmpr	0,x		Update CC based on the outcome
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

BALCHK	tst	BALNCD
	bne	BALERR
	rts
BALERR	ldb	#9		Illegal construct
	jsr	ERRHDLR		No return

SAVINP	pshs	x
	tfr	0,x
	tst	SRCID
	beq	@pushb
	ldx	BSBFADR
@pushb	bsr	RPUSH		Push 0 if SRCID is zero else BSBFADR
	ldx	UBLK
	bsr	RPUSH		Push BLK on the return stack
	ldx	UTOIN
	bsr	RPUSH		Push >IN on the return stack
	ldx	ISLEN
	bsr	RPUSH		Push ISLEN on the return stack
	puls	x
	rts

RSTINP	bsr	RPOP
	stx	ISLEN		Restore ISLEN from the return stack
	bsr	RPOP
	stx	UTOIN		Restore >IN from the return stack
	bsr	RPOP
	stx	UBLK		Restore BLK from the return stack
	bsr	RPOP
	clr	SRCID
	cmpr	0,x
	beq	@done
	stx	BSBFADR
	com	SRCID		Set SRCID to an NZ value
@done	rts

* Derive the current input stream pointer from BLK and >IN.
* The resulting address is returned in X. D is altered.
* The end of input stream address is re-computed.
* Both Y and W are preserved.
BKIN2PT	pshs	y
	ldx	UBLK
	beq	@notblk		Back to the console or a counted string
	pshsw
	jsr	NPUSH		Make sure BLK @ is loaded
	RFXT	jsr,BLOCK+8	XT for BLOCK
	pulsw
	UCNPOP			Retrieve buffer addr to X
@rsolvd	stx	BSBFADR		Update base buffer address
	tfr	x,y
	ldd	ISLEN
	leay	d,y
	sty	ISEADDR		Update the end of input stream address
	puls	y
	ldd	UTOIN
	leax	d,x		Add the current offset. Return the result via X
	rts
@notblk	ldx	#CMDBUF		Assume we are returning to the console
	tst	SRCID		Are we running under EVALUATE?
	beq	@rsolvd		No
	ldx	BSBFADR		We are returning to a counted string
	bra	@rsolvd

	include	rtc.asm		Experimental MC146818 support
	include	storage.asm	CompactFlash support

******************************************************************************
* Dictionary begins. Please note that @ has to be the first (in linked list
* order) word and EMPTY-BUFFERS has to be the last one. In the code below ANSI
* refers to ANSI-X3.215-1994 Draft 6 proposal (i.e. the free spec).

EBUFS	fcb	13		79-STANDARD (REQ145)
	fcc	'EMPTY-BUFFERS'	( -- )
	fdb	0		Last dictionary entry
	RFCS
EMPTYB	ldx	#BUF0
	bsr	EMPT1B
	ldx	#BUF1
* Empty the buffer pointed to by X.
EMPT1B	stx	MRUBUFA		Update most recently used buffer address
	leax	BOTERM,x	Buffer offset to the terminator field
	clrd
	std	,x		Clear terminator and flags fields
	IFNE	DEBUG
	ldd	#$C7C7
	std	2,x		Dummy block number
	ENDC			DEBUG
	rts

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
WBIFDRT	pshs	d
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
	leax	BOFLAGS,x
	lda	,x		Acquire the 'flags' field
	anda	#^BDIRTY	Clear the dirty bit
	sta	,x		and update the 'flags' field
@alldon	puls	x		Restore X
	puls	d		and D
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
	leax	BOFLAGS,x	Buffer 'flags' field's address to X
	lda	,x		Buffer 'flags' field to A
	bita	#BINUSE		Is that buffer in use?
	beq	@upddon		No, we're done here
	ora	#BDIRTY
	sta	,x		Set the dirty bit
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
@retba	stx	MRUBUFA		Block number match. Mark as the MRU buffer
	UCNPUSH			and return its base address via the data stack
	rts
@nxtbuf	leax	BFDISP,x	Point to the next buffer
	decb
	bne	@blkup
* The block number in Y is not currently in use. Assign a buffer to it.
	ldx	#BUF0
	cmpx	MRUBUFA		Most recently used buffer address
	bne	@bselct
	ldx	#BUF1
* At this point X has the base address of the block we are interested in.
@bselct	pshs	y		Backup the target block number
	jsr	WBIFDRT		Write back if dirty. X and D are preserved
	lda	#BINUSE
	sta	BOFLAGS,x	Update the buffer's 'flags' field
	puls	y		Restore the target block number
	sty	BOBLKNO,x	and update the 'blknum' field as well
	bra	@retba

BLOCK	fcb	5		79-STANDARD (REQ191)
	fcc	'BLOCK'		( ublkno -- addr )
	fdb	BUFFER
	RFCS
	RFXT	bsr,BUFFER+9	XT for BUFFER
* Upon return Y has has the block number.
	UCNPOP			Buffer base address to X
	pshs	x		Push base buffer address as Arg1 to CF1BKRD
	lda	BOFLAGS,x	Retrieve buffer 'flags' field
	IFNE	DEBUG
	bita	#BINUSE
	bne	@blkctd
	lda	#10		Assertion failed
	jsr	ERRHDLR		No return
	ENDC			DEBUG
@blkctd	anda	#BMAPPD		Has the block been read yet?
	bne	@bkmapd		Yes
	pshs	y		No. Push block number as arg0 to CF1BKRD
* Map in the block from the CF device. System stack structure is as follows:
* ,s has the target block number.
* 2,s has the buffer base address.
	jsr	CF1BKRD
	leas	2,s		Drop one cell from the system stack
* Update the buffer's flags field.
	ldx	,s		Base buffer address
	leax	BOFLAGS,x	Buffer 'flags' field address to X
	lda	,x		Read buffer 'flags' field
	ora	#BMAPPD
	sta	,x		and mark it as read
@bkmapd	puls	x		Buffer base address to X
	UCNPUSH
	rts

BLK	fcb	3
	fcc	'BLK'
	fdb	BLOCK
	RFCS
	ldx	#UBLK
	jmp	NPUSH

TOIN	fcb	3		ANSI (Core)
	fcc	'>IN'		( -- a-addr )
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
* Moved to CompactFlash screen #4.

* Functionally:
* : INDEX 1+ SWAP DO
*     CR   I SCR !
*     0 LINE 64 TYPE
*   LOOP ;
* Moved to CompactFlash screen #4.

TICKS	fcb	5		Non-standard
	fcc	'TICKS'		( -- tickslow tickshigh )
	fdb	SCR
	RFCS
	IFNE	RTCFEAT
	pshs	cc
	orcc	#FFLAG		Mask FIRQ while reading the double cell
	ldx	TIKSLOW
	ldy	TIKSHI
	puls	cc		Restore the previous interrupt handling mode
	jsr	NPUSH
	tfr	y,x
	ELSE
	tfr	0,x
	jsr	NPUSH
	ENDC			RTCFEAT
	jmp	NPUSH

RTCFTCH	fcb	4		Non-standard
	fcc	'RTC@'		( regoff -- byteval )
	fdb	TICKS
	RFCS
	IFNE	RTCFEAT
	tst	RTCAVL
	beq	RTNOCON
* An MC146818 RTC is present. Let's get down to business.
	jsr	NPOP		REGOFF to X
	tfr	x,d
	tfr	b,a
	jsr	RTREGRD
	clra			BYTEVAL to D
	pshu	d		Unchecked NPUSH of D
	rts
RTNOCON	ldb	#17		RTC not detected on bootup -> I/O error
	jsr	ERRHDLR		No return
	ELSE
	RFXT	jsr,DROP+7	XT for DROP
	tfr	0,x
	jmp	NPUSH
	ENDC			RTCFEAT

RTCSTOR	fcb	4		Non-standard
	fcc	'RTC!'		( byteval regoff -- )
	fdb	RTCFTCH
	RFCS
	IFNE	RTCFEAT
	tst	RTCAVL
	beq	RTNOCON
	jsr	MIN2PST		At least two cells need to be stacked up
	lda	1,u		REGOFF to A
	ldb	3,u		BYTEVAL to B
	leau	4,u		Drop two cells from the data stack
	jmp	RTREGWR
	ELSE
	RFXT	jmp,TWODROP+8	XT for 2 DROP
	ENDC			RTCFEAT

LIST	fcb	4		79-STANDARD (REQ109)
	fcc	'LIST'		( ublkno -- )
	fdb	RTCSTOR
	RFCS
	tst	CFCARDP
	bne	@cont
	ldb	#17		IO error
	jsr	ERRHDLR		No return
@cont	RFXT	jsr,DUP+6	XT for DUP
	RFXT	jsr,BLOCK+8	XT for BLOCK
* TOS now has the base buffer address.
	jsr	NPOP
	tfr	x,y		Base buffer address to Y
	jsr	NPOP		ublkno to X
	stx	USCR		Update SCR's value
	ldb	#16		16 lines to go
@loop	pshs	b
	jsr	PUTCR
	tfr	y,x
	jsr	NPUSH		Start address for TYPE
	ldx	#64
	jsr	NPUSH		Byte count for TYPE
	addr	x,y
	RFXT	jsr,TYPE+7	XT for TYPE
	puls	b
	decb
	bne	@loop
	rts

* This is supposed to align HERE (DICEND), presumably on a cell boundary.
* We have no such constraint on the 6309.
ALIGN	fcb	5		ANSI Core
	fcc	'ALIGN'		( -- )
	fdb	LIST
	RFCS
	rts

ALIGND	fcb	7		ANSI Core
	fcc	'ALIGNED'	( addr -- a-addr )
	fdb	ALIGN
	RFCS
	jmp	MIN1PST		At least one cell must be stacked up

CHARP	fcb	5		ANSI Core
	fcc	'CHAR+'		( c-addr1 -- c-addr2 )
	fdb	ALIGND
	RFCS
	RFXT	jmp,ONEP+5	XT for 1+

* n2 is the size in address units (bytes) of n1 characters. A NOOP.
CHARS	fcb	5		ANSI Core
	fcc	'CHARS'		( n1 -- n2 )
	fdb	CHARP
	RFCS
	jmp	MIN1PST		At least one cell must be stacked up

* Convert a single cell to a double. Non-transactional.
STOD	fcb	3		ANSI Core
	fcc	'S>D'		( n -- d )
	fdb	CHARS
	RFCS
	jsr	NPOP		N to X
	UCNPUSH			Push back low order cell
	clrd			High order cell: default to N >= 0
	exg	d,x
	tsta
	lbpl	NPUSH
	leax	-1,x		N is < 0. Sign extension is required. -1 to X
	jmp	NPUSH

NCLR	fcb	4		Non-standard
	fcc	'NCLR'		Clear the data (normal) stack
	fdb	STOD
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

DEPTH	fcb	5		ANSI (Core)
	fcc	'DEPTH'		( -- +n )
	fdb	RCLR
	RFCS
	ldd	#NSTBOT		Bottom data stack address
	subr	u,d		D has the current value of the data stack ptr
	lsrd			divided by 2
	tfr	d,x
	jmp	NPUSH		X to N

CREATE	fcb	6		ANSI (Core)
	fcc	'CREATE'	Comp: ( "<spaces>name" -- )
	fdb	DEPTH		Exec: ( -- a-addr )
	RFCS
	jsr	LOCWRT		Code entry point returned to Y
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

DOES	fcb	$C5		ANSI (Core)
	fcc	'DOES>'
	fdb	CREATE
	RFCS
	ldx	#DOESEX		JSR #DOESEX is compiled (no actual return)
	jmp	EMXASXT		Set as action component

DOESEX	ldx	LSTWAD		Header of the last dictionary entry
	ldb	,x+
	andb	#WRLNMSK	Extract word length
	abx
	IFNE	RELFEAT
	leax	9,x		2 bytes/backlink, 1/cksum, 6 bytes code offset
	ELSE
	leax	8,x		2 bytes/backlink, 6 bytes code offset
	ENDC			RELFEAT
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

LITERAL	fcb	$87		ANSI (Core)
	fcc	'LITERAL'	Comp: ( x -- )
	fdb	DOES		Exec: ( -- x )
	RFCS
	jsr	NPOP
	tst	USTATE+1
	bne	@comp
	rts			The standard defines no semantics in interp mode
@comp	ldy	DICEND
	lda	#LDXOPC		LDX immediate
	jsr	VARCON2		Compile LDX #X
	sty	DICEND		Update HERE
	ldx	#NPUSH
	jmp	EMXASXT		Set NPUSH as action component

* Functionally: : CONSTANT CREATE , DOES> @ ;
* The following code produces more compact code.
CONS	fcb	8		ANSI (Core)
	fcc	'CONSTANT'	Comp: ( x "<spaces>name" -- )
	fdb	LITERAL		Exec: ( -- x )
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
	ENDC			RELFEAT

* Functionally: : VARIABLE CREATE 2 ALLOT ;
* However we can save three bytes per instance with the following code.
VARI	fcb	8		ANSI (Core)
	fcc	'VARIABLE'	Comp: ( "<spaces>name" -- )
	fdb	CONS		Exec: ( -- a-addr )
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

IMMED	fcb	9		ANSI (Core)
	fcc	'IMMEDIATE'	( -- )
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

* Added for better support of ANSI VALUEs.
UNMON	fcb	9
	fcc	'UNMONITOR'	( -- )
	fdb	MONITOR
	RFCS
	IFNE	RELFEAT
	clra
	pshs	a
	bra	MONIT1
	ELSE
	rts
	ENDC			RELFEAT

* This non-standard word enables checkum monitoring by ICHECK for the
* last defined word in the dictionary. : words are monitored by default
* and so are constants. CREATEd words require an explicit invokation of
* MONITOR if they are to be checked for integrity.
MONITOR	fcb	7
	fcc	'MONITOR'	( -- )
	fdb	RSTRCT
	RFCS
	IFNE	RELFEAT
	lda	#1		Set MONFLM in the word 'flags' header field
	pshs	a
MONIT1	ldx	LSTWAD		Latest defined word header address
	lda	,x
	tst	,s		Should we set or clear MONFLM?
	bne	@setflg
	anda	#^MONFLM	Clear MONFLM in the word's attribute field
	bra	@cont
@setflg	ora	#MONFLM		Set MONFLM in the word's attribute field
@cont	sta	,x
	bsr	HDRSKIP		Skip the header (XT to X), clear A
	ldy	DICEND		The upper code section limit (excluded)
	bsr	HDRCSUM		Current word's definition's checksum to A
	sta	-1,x		Store the computed checksum into the header
	leas	1,s		Drop one byte from the system stack
	ENDC			RELFEAT
	rts

	IFNE	RELFEAT
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

CSUMFLM	fcn	'integrity check failed'
	ENDC			RELFEAT

* This non-standard word walks through the dictionary linked list and checks
* that the words that have the MONFLM flag set in their header's attribute
* field have a definition that still matches the checksum stored in the word's
* header. This is meant to be used interactively, as a debugging tool, since
* multitasking is not supported by this implementation. Therefore no status
* value is returned. Feedback will only be provided for corrupted words, in
* the form of a diagnostic message printed to the console.
ICHECK	fcb	6
	fcc	'ICHECK'	( -- )
	fdb	UNMON
	RFCS
	IFNE	RELFEAT
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
	ENDC			RELFEAT
@ickdon	rts

* : ?DO 0 rakeVar !
*   POSTPONE do?DO
*   HERE            \ leave ?DO-orig
*   0 ,
*   HERE            \ leave DO-dest
*   bal+ ; IMMEDIATE RESTRICT
QDO	fcb	$C3		ANSI (Core ext)
	fcc	'?DO'		Comp: ( C: -- do-sys )
	fdb	ICHECK		Exec: ( n1|u1 n2|u2 -- ) ( R: -- | loop-sys )
	RFCS
	ldx	#QDOEX
	jsr	EMXASXT		Compile "JSR QDOEX"
* The rest of this code looks very much like IF, except that 1 is not pushed
* to the control flow stack to indicate an IF. This is done later on when
* the RAKE code is executed by LOOP.
	ldd	#BNEOPC
	std	,y++
	lda	#JMPOPC
	sta	,y+
	tfr	y,x
	jsr	CSPUSH		ANS:do-sys/addr (?DO-orig) is HERE
	leay	2,y		2 ALLOT instead of 0 ,
	sty	DICEND
	tfr	y,x		ANS:do-sys/type (DO-dest) is HERE
	jsr	CSPUSH
QDO1	clrd
	std	RAKEVAR		Used for LEAVE forward references handling
	inc	BALNCD
	rts

QDOEX	jsr	MIN2PST
	ldx	2,u		Loop limit
	ldy	,u		Loop index
	leau	4,u		2DROP
	cmpr	y,x
	beq	@skloop		Loop bypassed, return with ZFLAG set
	jsr	RPUSH		limit >R
	tfr	y,x
	jsr	RPUSH		index >R
	andcc	#^ZFLAG		Clear ZFLAG
@skloop	rts

* : DO 0 rakeVar !   0   POSTPONE doDO   HERE   bal+ ; IMMEDIATE RESTRICT
DO	fcb	$C2		ANSI (Core)
	fcc	'DO'		Comp: ( C: -- do-sys )
	fdb	QDO		Exec: ( n1|u1 n2|u2 -- ) ( R: -- loop-sys )
	RFCS
	ldx	#DOEX
	jsr	EMXASXT		Compile "JSR DOEX"
	tfr	0,x		ANS:do-sys/addr (?DO-orig) is 0 for DO
	jsr	CSPUSH
	tfr	y,x		ANS:do-sys/type (DO-dest) is HERE
	jsr	CSPUSH
	bra	QDO1

DOEX	RFXT	jsr,SWAP+7	XT for SWAP
	RFXT	jsr,TOR+5	XT for >R (limit)
	RFXT	jmp,TOR+5	XT for >R (index)

* : LOOP POSTPONE doLOOP   rake ; IMMEDIATE RESTRICT
LOOP	fcb	$C4		ANSI (Core)
	fcc	'LOOP'		Comp: ( C: do-sys -- )
	fdb	DO		Exec: ( -- ) ( R:  loop-sys1 --  | loop-sys2 )
	RFCS
	ldx	#LOOPEX
LOOP1	jsr	EMXASXT
	ldx	#BCSOPC		Compile "BCS *+5"
	stx	,y++
	jsr	CSPOP		ANS:do-sys/type (DO-dest): loop begin. addr.
	lda	#JMPOPC
	jsr	VARCON2		Compile "JMP DO-dest"
	sty	DICEND
	sty	FWDREF		Last recorded forward reference
* RAKE: Y has HERE, which all (if any) LEAVE forward references
* should resolve to.
	ldx	RAKEVAR
@lopres	beq	@lopdon
	ldd	,x		D has the next forward reference
	sty	,x		Resolve LEAVE forward reference
	tfr	d,x
	tstd
	bra	@lopres
@lopdon	std	RAKEVAR
	dec	BALNCD
	jsr	CSPOP		ANS:do-sys/addr (?DO-orig) to X
	bne	@endqdo		If NZ push it back, push type 1 and call THEN
	rts
* End a ?DO construct with an implicit THEN.
@endqdo	inc	BALNCD
	jsr	CSPUSH		Push back IF jump address 
	ldx	#1
	jsr	CSPUSH		to the control flow stack with type 1 (IF)
	RFXT	jmp,THEN+7

LOOPEX	ldx	#1
	bra	PLOPEX1

PLOOP	fcb	$C5		ANSI (Core)
	fcc	'+LOOP'		Comp: ( C: do-sys -- )
	fdb	LOOP		Exec: ( n -- ) ( R: loop-sys1 -- | loop-sys2 )
	RFCS
	ldx	#PLOOPEX
	bra	LOOP1

* Anton Ertl's forth-standard.org notes on +LOOP (2019-05-21 05:54:21):
* "Note that the loop control parameters can be either signed or unsigned,
* and +LOOP has to work for both. For systems with 2s-complement representation
* for signed numbers, the way to go is to use circular arithmetic: compute
* x=(index-limit)+minint, and observe if the addition x+n crosses the boundary
* between minint and maxint. Many architectures report this through the
* overflow flag."
* Here we do precisely what the good Doktor says, setting the carry flag on
* overflow detection.
PLOOPEX	jsr	NPOP
PLOPEX1	tfr	x,w		Increment to W
	jsr	RPOP
	tfr	x,y		Index to Y
	jsr	RPOP		Limit to X
	ldd	#$8000		Minimum integer on a 2 byte cell system
	addr	y,d		add the index
	subr	x,d		substract the limit
	addr	w,d		add the increment and check for overflow
	bvs	@limrcd		Limit reached
	jsr	RPUSH		Limit to the return stack
	addr	w,y		Update the index
	tfr	y,x
	jsr	RPUSH		Updated index to the return stack
	andcc	#^CFLAG		Clear CFLAG
	rts
@limrcd	orcc	#CFLAG		Set CFLAG
	rts

UNLOOP	fcb	$46		ANSI (Core)
	fcc	'UNLOOP'	( -- ) ( R: loop-sys -- )
	fdb	PLOOP
	RFCS
	jsr	RPOP		Drop the index from the return stack
	jmp	RPOP		and the loop limit as well

* Prototyping code below:
*
* VARIABLE bal   0 bal !
* : bal+ 1 bal +! ;
* : bal- -1 bal +! ;
* : branch jmpopc C, ; RESTRICT
* : 0branch jsropc C, npop ,
*   bneopc ,
*   branch ; RESTRICT
* : AHEAD branch HERE 0 ,  \ 0 is an unresolved forward reference
*   bal+ 1 ; IMMEDIATE RESTRICT
AHEAD	fcb	$C5		ANSI (Tools ext)
	fcc	'AHEAD'		Comp: ( C: -- orig )
	fdb	UNLOOP		Exec: ( -- )
	RFCS
	ldy	DICEND
AHEAD1	lda	#JMPOPC
	sta	,y+
	tfr	y,x		Jump address location (ANS:orig/addr) to X
	leay	2,y
	sty	DICEND		2 ALLOT (instead of 0 ,)
	inc	BALNCD
	jsr	CSPUSH		ANS:orig/addr to the control flow stack
	ldx	#1		ANS:orig/type is 1
	jmp	CSPUSH

* : IF 0branch HERE 0 ,  \ 0 is an unresolved forward reference
*   bal+ 1 ; IMMEDIATE RESTRICT
IF	fcb	$C2		ANSI (Core)
	fcc	'IF'		Comp: ( C: -- orig )
	fdb	AHEAD		Exec: ( x -- )
	RFCS
	ldx	#NPOP
	jsr	EMXASXT		Compile "JSR NPOP"
	ldd	#BNEOPC
	std	,y++		Compile "BNE *+5"
	bra	AHEAD1

* Functionally equivalent to:
* : UNLESS POSTPONE 0= POSTPONE IF ; IMMEDIATE RESTRICT
UNLESS	fcb	$C6		Non-standard (Perl inspired)
	fcc	'UNLESS'
	fdb	IF
	RFCS
	RFXT	ldx,#NULP+5	XT for 0=
	jsr	EMXASXT
	RFXT	bra,IF+5	XT for IF

* : ELSE POSTPONE AHEAD 2SWAP POSTPONE THEN ; IMMEDIATE RESTRICT
ELSE	fcb	$C4		ANSI (Core)
	fcc	'ELSE'		Comp: ( C: orig1 -- orig2 )
	fdb	UNLESS		Exec: ( -- )
	RFCS
	RFXT	bsr,AHEAD+8
	RFXT	jsr,TWOSWAP+8	This should be read as "1 CS-ROLL"
	RFXT	bra,THEN+7

* : THEN 1- ABORT" Unbalanced IF/ELSE/THEN construct"
*   HERE SWAP ! bal-
*   HERE fwdref ! ; IMMEDIATE RESTRICT
THEN	fcb	$C4		ANSI (Core)
	fcc	'THEN'		Comp: ( C: orig -- )
	fdb	ELSE		Exec: ( -- )
	RFCS
	jsr	CSPOP		ANS:orig/type to X
	leax	-1,x
	lbne	BALERR		Illegal construct, type must be 1
	ldy	DICEND
	jsr	CSPOP		ANS:orig/addr to X
	sty	,x		Resolve forward reference to HERE
	sty	FWDREF		Last recorded forward reference
	dec	BALNCD
	rts

EQ	fcb	1		ANSI (Core)
	fcc	'='		( x1 x2 -- flag )
	fdb	THEN
	RFCS
	jsr	MIN2PST		At least two cells need to be stacked up
	ldq	,u		D:W has X2:X1
	leau	2,u		Drop one cell from the user stack
	tfr	0,x
	cmpr	w,d
	bne	@eq1
	leax	-1,x		Return the ANSI true
@eq1	stx	,u		Store in place to FLAG
	rts

DIFF	fcb	2		79-STANDARD (REF)
	fcc	'<>'		( N1 N2 -- FLAG )
	fdb	EQ
	RFCS
	jsr	MIN2PST		At least two cells need to be stacked up
	ldq	,u		D:W has N2:N1
	leau	2,u		Drop one cell from the user stack
	tfr	0,x
	cmpr	w,d
	beq	@diff1
	leax	-1,x		Return the ANSI true
@diff1	stx	,u		Store in place to FLAG
	rts

SINFEQ	fcb	2		Non-standard (Not even ANSI!)
	fcc	'<='		Required for the ORNL fixed sieve benchmark
	fdb	DIFF
	RFCS
	jsr	MIN2PST		At least two cells need to be stacked up
	ldy	,u
	ldx	2,u
	clrd
	cmpr	y,x
	bgt	@sinfq1
	decd			Return the ANSI true
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
	ENDC			DEBUG

XOR	fcb	3		ANSI (Core)
	fcc	'XOR'		( x1 x2 -- x3 )
	IFNE	DEBUG
	fdb	CC
	ELSE
	fdb	SINFEQ
	ENDC			DEBUG
	RFCS
	jsr	MIN2PST		At least two cells need to be stacked up
	ldd	,u		X2 to D
	ldw	2,u		X1 to W
	eorr	w,d
XOR1	leau	2,u
	std	,u		X1 ^ X2 to X3
	rts

OR	fcb	2		ANSI (Core)
	fcc	'OR'		( x1 x2 -- x3 )
	fdb	XOR
	RFCS
	jsr	MIN2PST		At least two cells need to be stacked up
	ldd	,u		X2 to D
	ldw	2,u		X1 to W
	orr	w,d
	bra	XOR1		X1 | X2 to X3

AND	fcb	3		ANSI (Core)
	fcc	'AND'		( x1 x2 -- x3 )
	fdb	OR
	RFCS
	jsr	MIN2PST		At least two cells need to be stacked up
	ldd	,u		X2 to D
	ldw	2,u		X1 to W
	andr	w,d
	bra	XOR1		X1 & X2 to X3

INVERT	fcb	6		ANSI (Core)
	fcc	'INVERT'	( x1 -- x2 )
	fdb	AND
	RFCS
	jsr	NPOP		X1 to X
	tfr	x,d
	comd
	tfr	d,x		X2 to X
	UCNPUSH			and to the data stack
	rts

ZGREAT	fcb	2		79-STANDARD (REQ118)
	fcc	'0>'
	fdb	INVERT
	RFCS
	jsr	NPOP
	tfr	x,d
	tstd
	ble	@zgrt1
	ldx	#-1		Return the ANSI true
	UCNPUSH
	rts
@zgrt1	tfr	0,x
	UCNPUSH
	rts

ZLESS	fcb	2		ANSI (Core)
	fcc	'0<'		( n -- flag )
	fdb	ZGREAT
	RFCS
	jsr	NPOP
	tfr	x,d
	tstd
	bge	@zlss1
	ldx	#-1		Return the ANSI true
	UCNPUSH
	rts
@zlss1	tfr	0,x
	UCNPUSH
	rts

NULP	fcb	2		ANSI (Core)
	fcc	'0='		( x -- flag )
	fdb	ZLESS
	RFCS
	jsr	NPOP
	tfr	x,d
	tfr	0,x
	tstd
	beq	@nulp2
@nulp1	UCNPUSH
	rts
@nulp2	leax	-1,x		Return the ANSI true
	bra	@nulp1

ZNEQ	fcb	3		ANSI (Core ext)
	fcc	'0<>'
	fdb	NULP
	RFCS
	RFXT	bsr,NULP+5
	RFXT	bra,INVERT+9

* Maybe this one should go. It is not specified in the ANS94 reference
* document but Conklin/Rather have it as "common usage."
NOT	fcb	3		79-STANDARD (REQ165)
	fcc	'NOT'
	fdb	ZNEQ
	RFCS
	RFXT	bra,NULP+5	XT for 0=

USUP	fcb	2		ANSI (Core Ext)
	fcc	'U>'
	fdb	NOT
	RFCS
	jsr	CMP2
	bls	@usup1
	leax	-1,x		Return the ANSI true
@usup1	UCNPUSH
	rts

UINF	fcb	2		ANSI (Core)
	fcc	'U<'		( u1 u2 -- flag )
	fdb	USUP
	RFCS
	jsr	CMP2
	bhs	@uinf1		Z is set
	leax	-1,x		Return the ANSI true
@uinf1	UCNPUSH
	rts

SUP	fcb	1		ANSI (Core)
	fcc	'>'		( n1 n2 -- flag )
	fdb	UINF
	RFCS
	jsr	CMP2
	ble	@sup1
	leax	-1,x		Return the ANSI true
@sup1	UCNPUSH
	rts

INF	fcb	1		ANSI (Core)
	fcc	'<'		( n1 n2 -- flag )
	fdb	SUP
	RFCS
	jsr	CMP2
	bge	@inf1
	leax	-1,x		Return the ANSI true
@inf1	UCNPUSH
	rts

MAX	fcb	3		ANSI (Core)
	fcc	'MAX'		( n1 n2 -- n3 )
	fdb	INF
	RFCS
	jsr	NPOP
	tfr	x,y		N2 to Y
	jsr	NPOP		N1 to X
	cmpr	y,x
	bge	@pshrv1
	tfr	y,x
@pshrv1	UCNPUSH			(X) N3 to the data stack
	rts

MIN	fcb	3		ANSI (Core)
	fcc	'MIN'		( n1 n2 -- n3 )
	fdb	MAX
	RFCS
	jsr	NPOP
	tfr	x,y		N2 to Y
	jsr	NPOP		N1 to X
	cmpr	y,x
	ble	@pshrv2
	tfr	y,x
@pshrv2	UCNPUSH			(X) N3 to the data stack
	rts

ABS	fcb	3		ANSI (Core)
	fcc	'ABS'		( n -- u )
	fdb	MIN
	RFCS
	jsr	NPOP
	tfr	x,d		N to D
	tstd
	bpl	@abs1
	negd
	tfr	d,x
@abs1	UCNPUSH			X to U
	rts

NEGATE	fcb	6		ANSI (Core)
	fcc	'NEGATE'	( n1 -- n2 )
	fdb	ABS
	RFCS
	jsr	MIN1PST		At least one cell needs to be stacked up
	ldd	,u
	negd
	std	,u
	rts

* : BEGIN HERE 0 bal+ ; IMMEDIATE RESTRICT
BEGIN	fcb	$C5		ANSI (Core)
	fcc	'BEGIN'		Comp: ( C: -- dest )
	fdb	NEGATE		Exec: ( -- )
	RFCS
	inc	BALNCD
	ldx	DICEND		HERE is ANS:dest/addr
	jsr	CSPUSH		to the control flow stack
	tfr	0,x		ANS:dest/type is zero
	jmp	CSPUSH		to the control flow stack

* : AGAIN ABORT" Unbalanced BEGIN/AGAIN construct"
*   branch , bal- ; IMMEDIATE RESTRICT
AGAIN	fcb	$C5		ANSI (Core ext)
	fcc	'AGAIN'		Comp: ( C: dest -- )
	fdb	BEGIN		Exec: ( -- )
	RFCS
	jsr	CSPOP		ANS:dest/type to X (CC is set)
	lbne	BALERR		type must be zero
	jsr	CSPOP		ANS:dest/addr to X
	ldy	DICEND
	sty	JSRLAST
AGAIN1	lda	#JMPOPC		JMP extended
	jsr	VARCON2
	sty	DICEND
	dec	BALNCD
	rts

* The standard does not require this as being immediate but I do.
EXIT	fcb	$C4		ANSI (Core)
	fcc	'EXIT'		( -- )
	fdb	AGAIN
	RFCS
	ldx	DICEND
	leax	-3,x
	ldy	JSRLAST
	cmpr	y,x
	bne	@noopt		Tail JMP optimization is not possible
	lda	#JMPOPC		JMP extended
	sta	,x
	rts
@noopt	leax	3,x		Point back to HERE
	lda	#RTSOPC		RTS inherent
	sta	,x+
	stx	DICEND
	rts

* : UNTIL ABORT" Unbalanced BEGIN/UNTIL construct"
*   0branch , bal- ; IMMEDIATE RESTRICT
UNTIL	fcb	$C5		ANSI (Core)
	fcc	'UNTIL'		Comp: ( C: dest -- )
	fdb	EXIT		Exec: (x -- )
	RFCS
	jsr	CSPOP		ANS:dest/type to X (CC is set)
	lbne	BALERR		type must be zero
	ldx	#NPOP
	jsr	EMXASXT		Compile "JSR NPOP"
	ldx	#BNEOPC		Compile "BNE *+5"
	stx	,y++
	jsr	CSPOP		ANS:dest/addr to X
	bra	AGAIN1

* : WHILE POSTPONE IF 2SWAP ; RESTRICT IMMEDIATE
WHILE	fcb	$C5		ANSI (Core)
	fcc	'WHILE'		Comp: ( C: dest -- orig dest )
	fdb	UNTIL		Exec: ( x -- )
	RFCS
	RFXT	jsr,IF+5
	RFXT	jmp,TWOSWAP+8	This should be read as "1 CS-ROLL"

* : REPEAT POSTPONE AGAIN POSTPONE THEN ; RESTRICT IMMEDIATE
REPEAT	fcb	$C6		ANSI (Core)
	fcc	'REPEAT'	Comp: ( C: orig dest -- )
	fdb	WHILE		Exec: ( -- )
	RFCS
	RFXT	bsr,AGAIN+8
	RFXT	jmp,THEN+7

RFROM	fcb	$42		ANSI (Core)
	fcc	'R>'		( -- x ) ( R:  x -- )
	fdb	REPEAT
	RFCS
	jsr	RPOP
	jmp	NPUSH

TOR	fcb	$42		ANSI (Core)
	fcc	'>R'		( x -- ) ( R:  -- x )
	fdb	RFROM
	RFCS
	jsr	NPOP
	jmp	RPUSH

* : LEAVE POSTPONE UNLOOP POSTPONE branch
*   HERE rakeVar DUP @ , ! ; IMMEDIATE RESTRICT
* rakeVar points to the head of a linked list of forward references to
* be resolved later by LOOP/+LOOP.
LEAVE	fcb	$C5		ANSI (Core)
	fcc	'LEAVE'		Exec: ( -- ) ( R: loop-sys -- )
	fdb	TOR
	RFCS
	RFXT	ldx,#UNLOOP+9
	jsr	EMXASXT		POSTPONE UNLOOP
	lda	#JMPOPC
	sta	,y+		POSTPONE branch
	tfr	y,x		save HERE (fwd ref. to be resolved by LOOP)
	ldd	RAKEVAR
	std	,y++		rakeVar @ ,
	stx	RAKEVAR		HERE rakeVar !
	sty	DICEND
	rts

INDI	fcb	$41		ANSI (Core)
	fcc	'I'		( -- n|u ) ( R:  loop-sys -- loop-sys )
	fdb	LEAVE
	RFCS
	clrb
RPICKN	lda	RDEPTH
	cmpr	a,b
	bhs	@rpick1
	ldx	RSP
	clra
	lsld			Times 2
	ldx	d,x
	jmp	NPUSH		We cannot use UCNPUSH here
@rpick1	ldb	#8		Return stack underflow
	jsr	ERRHDLR		No return

RFETCH	fcb	$42		ANSI (Core)
	fcc	'R@'		( -- x ) ( R:  x -- x )
	fdb	INDI
	RFCS
	RFXT	bra,INDI+4	XT for I

INDIP	fcb	$42		79-STANDARD (REF)
	fdb	$4927
	fdb	RFETCH
	RFCS
	ldb	#1
	bra	RPICKN

INDJ	fcb	$41		ANSI (Core)
	fcc	'J'		Exec: ( -- n|u ) ( R: lsy1 lsy2 -- lsy1 lsy2 )
	fdb	INDIP
	RFCS
	ldb	#2
	bra	RPICKN

INDJP	fcb	$42		Non-standard
	fdb	$4A27
	fdb	INDJ
	RFCS
	ldb	#3
	bra	RPICKN

INDK	fcb	$41		79-STANDARD (REF)
	fcc	'K'
	fdb	INDJP
	RFCS
	ldb	#4
	bra	RPICKN

QUIT	fcb	4		ANSI (Core)
	fcc	'QUIT'		( -- )  ( R:  i*x -- )
	fdb	INDK
	RFCS
	clr	USTATE+1
	RFXT	jsr,RCLR+7	XT for RCLR
	lds	#RAMSTRT+RAMSIZE Reset the system stack pointer
	jsr	PUTCR
	jmp	INTERP

ABORT	fcb	5		ANSI (Core)
	fcc	'ABORT'		( i*x -- ) ( R: j*x -- )
	fdb	QUIT
	RFCS
	ldb	#3		User ABORT
	jsr	ERRHDLR		No return

* Implementation notes: GNU Forth, VFX and SwiftForth all report "invalid
* memory address" for "0 FIND". Also the counted string at c-addr is not
* scanned for leading spaces. If the counted string byte count is zero, the
* string will be not found and the dictionary not searched at all.
FIND	fcb	4		ANSI (Core)
	fcc	'FIND'		( c-addr -- c-addr 0  |  xt 1  |  xt -1 )
	fdb	ABORT
	RFCS
	jsr	MIN1PST
	ldx	,u		TOS to X (Arg <c-addr>)
	bne	@afind1
	ldb	#13		Illegal argument
	jsr	ERRHDLR		No return
@afind1	tst	,x
	bne	@afind3		Character count is NZ, proceed
@afind2	tfr	0,x		Word not found
	jmp	NPUSH
@afind3	ldd	TOKENSP
	pshs	d		Save TOKENSP to the system stack
	ldb	,x+
	stb	CURTOKL
	stx	TOKENSP
	jsr	SWDIC1
	ldd	PLOAD		Retrieve word payload
	std	FNDPLD		Make it accessible through PAYLOAD
	puls	d
	std	TOKENSP		Restore TOKENSP from the system stack
* If the word is not found, Y is 0 and we ought to branch back to that context.
	cmpr	0,y
	beq	@afind2
* Word was found, push the corresponding XT (in Y) to the data stack (in place).
	sty	,u
	ldx	#-1		Assume non-immediate
	tst	IMDFLG
	beq	@afind4
	leax	2,x		#1 to X
@afind4	jmp	NPUSH

RBRACK	fcb	1		ANSI (Core)
	fcc	']'		( -- )
	fdb	FIND
	RFCS
	lda	#-1
RBRACK1	sta	USTATE+1
	rts

* Note: the standard does not mandate that this primitive be executed in
* definitions only. IMHO, it ought to, therefore I am forcing the C bit here.
LBRACK	fcb	$C1		ANSI (Core)
	fcc	'['		( -- )
	fdb	RBRACK
	RFCS
	clra
	bra	RBRACK1

* Functionally:
* : ' BL WORD FIND IF
*     EXIT                      \ XT is left on the data stack
*   THEN
*   DROP 0 ;
* There is a little extra complexity here because the standard requires
* an error condition to be triggered if the word is not found.
TICK	fcb	1		ANSI (Core)
	fcb	$27		( "<spaces>name" -- xt )
	fdb	LBRACK
	RFCS
	RFXT	jsr,BL+5
	RFXT	jsr,WORD+7
	RFXT	bsr,FIND+7
* We have at least two cells returned by the ANS94 FIND on the data stack.
	ldd	,u		TOS to D (ANS94 FIND flag)
	beq	@nfound		Target word was not found
	leau	2,u		Drop the flag and return the XT
	rts
@nfound	leau	4,u		Drop two cells from the data stack
	ldx	TOKENSP
	jsr	SCNSTOK		Needed to skip leading spaces
	ldb	#2		Word not found
	jsr	ERRHDLR		No return

* Functionally: : ['] ' POSTPONE LITERAL ; IMMEDIATE RESTRICT
BKQUOT	fcb	$C3		ANSI (Core)
	fcb	$5B,$27,$5D	Comp: ( "<spaces>name" -- )
	fdb	TICK		Exec: ( -- xt )
	RFCS
	RFXT	bsr,TICK+4	XT for '
* Data stack topmost cell has the target word address.
	RFXT	jmp,LITERAL+10	XT for LITERAL

POSTPON	fcb	$C8		ANSI (Core) Not a straight alias to [COMPILE]
	fcc	'POSTPONE'	( "<spaces>name" -- )
	fdb	BKQUOT		Non-immediate words deserve special treatment
	RFCS
	jsr	BKIN2PT		Derive X from BLK, >IN
	jsr	SCNSTOK
	bne	@postp1
	ldb	#5		Missing word name
	jsr	ERRHDLR		No return
@postp1	jsr	SWDIC
	bne	@postp2		Word found. Code address returned in Y
	ldx	TOKENSP
	ldb	#2		Undefined (X points to the offending word)
	jsr	ERRHDLR		No return
@postp2	tfr	y,x		X has the actual execution token
	tst	IMDFLG
	beq	@postp4		Target word is not immediate
@postp3	jsr	EMXASXT		Set as action component
	ldd	TOKENSP		Updated by SWDIC if the word was found
	jmp	U2INFRD		Derive >IN from D
* The word being considered is non-immediate. The equivalent input should be:
* ['] <word> COMPILE, We have the XT for <word> in X.
@postp4	jsr	LITER
	RFXT	ldx,#CMPCOMA+11	XT for COMPILE,
	bra	@postp3

* GNU Forth has this as non-immediate so I am going for it as well.
CMPCOMA	fcb	$48		ANSI (Core Ext)
	fcc	'COMPILE,'	( XT -- )
	fdb	POSTPON
	RFCS
	jsr	NPOP		Execution token to X
	jmp	EMXASXT

* As per the standard, : is not immediate. This allows for further interesting
* developments, like tracing words execution...
COMPC	fcb	$1		ANSI (Core)
	fcc	':'		Comp: ( "<spaces>name" -- )
	fdb	CMPCOMA
	RFCS
	clr	ANCMPF
COMPC1	ldd	#-1
	sta	USTATE+1
	comd			0 to D
	sta	BALNCD
	std	JSRLAST
	std	FWDREF
	tst	ANCMPF		Anonymous compilation?
	bne	@isanon
	jmp	LOCWRT
@isanon	ldx	DICEND
	stx	BDICEND		Backup HERE
	stx	RECADDR		Should RECURSE by used by a :NONAME definition
	rts

NONAME	fcb	$7
	fcc	':NONAME'
	fdb	COMPC
	RFCS
	lda	#1
	sta	ANCMPF		Set the anonymous compilation flag
	bra	COMPC1

* Tail call optimization notes:
* 1: if JSRLAST is 0, emit an RTS, the end.
* 2: if HERE - 3 == JSRLAST: replace JSR by a JMP.
* 3: if FWDREF == HERE, emit an RTS.
* The end means finalize with DEBUG code and an update of HERE (DICEND).
COMPR	fcb	$C1		ANSI (Core)
	fcc	';'
	fdb	NONAME
	RFCS
	jsr	BALCHK		Check for unbalanced constructs
	clr	USTATE+1	Back to interpretation mode
* Do not restore LSTWAD if we came from :NONAME.
	ldx	BDICEND		X as HERE when : (LOCWRT) or :NONAME was called
	tst	ANCMPF
	bne	@wasano
	stx	LSTWAD		Update LAST
	bra	@cont
@wasano	clr	ANCMPF
	jsr	NPUSH		Anonynous execution token to the data stack
@cont	ldx	DICEND		HERE to X
* Optimization: replace the last JSR by a JMP, if possible.
	ldd	JSRLAST
	beq	@rtsreq		Case #1
	leay	-3,x		Y has HERE - 3, D has JSRLAST
	cmpr	d,y
	bne	@rtsreq
* Tail call optimization applies (Case #2).
	lda	#JMPOPC
	sta	,y
	ldy	FWDREF
	cmpr	x,y
	bne	@finalz		Case #3
@rtsreq	lda	#RTSOPC		RTS inherent
	sta	,x+
@finalz
	IFNE	DEBUG
	lda	#ILLOPC		Illegal opcode
	sta	,x+
	ENDC			DEBUG
	stx	DICEND		Update HERE
	IFNE	RELFEAT
	RFXT	jsr,MONITOR+10	XT for MONITOR. All : words are candidates
*				for integrity check by ICHECK.
	ENDC			RELFEAT
	rts

RECURSE	fcb	$C7		ANSI (Core)
	fcc	'RECURSE'	( -- )
	fdb	COMPR
	RFCS
	ldx	RECADDR		Set up by LOCWRT
	jmp	EMXASXT		Set as action component

MARKER	fcb	6		ANSI (Tools ext)
	fcc	'MARKER'
	fdb	RECURSE
	RFCS
	ldy	DICEND		HERE
	ldx	LSTWAD		LAST
	pshs	y,x		Preserve essential dictionary pointers
	jsr	LOCWRT		No pre-req on input, does all the heavy lifting

* From here on, all we need to do is to emit the code necessary to restore
* DICEND (2,s) and LSTWAD (,s). Y has HERE, as of right now.
	ldx	2,s		Saved HERE to X
	lda	#LDXOPC
	jsr	VARCON2
	ldd	#($9F*256)|(DICEND-VARSPC)
	std	,y++		Compile stx DICEND (direct page)

	ldx	,s		Saved LAST to X
	lda	#LDXOPC
	jsr	VARCON2
	ldd	#($9F*256)|(LSTWAD-VARSPC)
	std	,y++		Compile stx LSTWAD (direct page)

	lda	#RTSOPC
	sta	,y+		Emit an RTS
	leas	4,s		Drop material previously on the system stack
	jmp	CREAT1		Finalize dictionary entry

EXCT	fcb	7		ANSI (Core)
	fcc	'EXECUTE'	( i*x xt -- j*x )
	fdb	MARKER
	RFCS
	jsr	NPOP		Although the standard does not specify that
	beq	@exct1		a NUL address should trigger an error, I do
	tfr	x,pc		Branch to the XT
@exct1	ldb	#13		Illegal argument
	jsr	ERRHDLR		No return

BYE	fcb	3		ANSI (Programming tools)
	fcc	'BYE'
	fdb	EXCT
	RFCS
	IFNE	RTCFEAT
	lda	#RTOREGB
	jsr	RTREGRD		RTCB register to B
	andb	#^RTBPIE	Disable periodic interrupt generation
	jsr	RTREGWR
	ldx	#40
	jsr	MILLIS1		Wait for 40 milliseconds
	orcc	#(FFLAG|IFLAG)	Disable maskable interrupts
	ENDC			RTCFEAT
	jmp	RSTHDL

BKCHAR	fcb	$C6		ANSI (Core)
	fcc	'[CHAR]'	Comp: ( "<spaces>name" -- )
	fdb	BYE		Exec: ( -- char )
	RFCS
	RFXT	bsr,CHAR+7	XT for CHAR
	RFXT	jmp,LITERAL+10	XT for LITERAL

CHAR	fcb	4		ANSI (Core)
	fcc	'CHAR'		( "<spaces>name" -- char )
	fdb	BKCHAR
	RFCS
	jsr	BKIN2PT		Derive X from BLK, >IN
@char1	jsr	SCNSTOK		X points to the beginning of the character
	beq	@chrerr
	ldb	,x
	clra			D has CHAR
	tfr	d,x
	jsr	NPUSH
	ldx	TOKENSP		Set by SCNSTOK
	jsr	SCNETOK
	tfr	x,d		TOKENEP
	jmp	U2INFRD		Derive >IN from D
@chrerr	ldb	#13		Illegal argument
	jsr	ERRHDLR
* No return.

* Hairy code but working.
WORD	fcb	4		ANSI (Core)
	fcc	'WORD'		( char "<chars>ccc<char>" -- c-addr )
	fdb	CHAR
	RFCS
	jsr	NPOP
	tfr	x,w		F has the delimiter ASCII code
	ldy	DICEND		The counted string returned is stored at HERE
	pshs	y
	clr	,y+		Initialize its length
        jsr	BKIN2PT		Derive X from BLK, >IN
	jsr	NXTCHAR		Current input char to A unless ZFLAG is set
	beq	@word3
	cmpa	#SP		BL
	beq	@word1		Skip initial blank if there is one
	leax	-1,x		Go back one char.
@word1	jsr	NXTCHAR
	beq	@word3		EOL reached, this is the end
	cmpr	f,a		Leading delimiter matched?
	beq	@word1		Yes, skip it (it might be repeated)
* Either there was no leading delimiter or we went past the leading repetitions.
	leax	-1,x		Go back one char.
@word2	jsr	NXTCHAR		Acquire next character from the input stream
	beq	@word3		EOL reached
	cmpr	f,a		Trailing delimiter?
	beq	@word3
	sta	,y+
	inc	[,s]		Increment string length
	bra	@word2
@word3	tfr	x,d		Pointing one char after the delimiter or EOIS
	jsr	U2INFRD		Derive >IN from D
	puls	x
	UCNPUSH			Push back HERE
	rts

LPAR	fcb	$81		ANSI (Core). No longer 79-STANDARD compliant
	fcc	'('		Exec: ( "ccc<paren>" -- )
	fdb	WORD
	RFCS
	ldx	#')
	jsr	NPUSH
	RFXT	bsr,WORD+7
	RFXT	jmp,DROP+7

SOURCE	fcb	6		ANSI (Core)
	fcc	'SOURCE'	( -- c-addr u )
	fdb	LPAR
	RFCS
	ldx	BSBFADR
	jsr	NPUSH
	ldx	ISLEN		Input stream length
	jmp	NPUSH

* This is a straightforward implementation borrowed from GNU Forth 'see \':
* : \
*   BLK @
*   IF     >IN @ C/L / 1+ C/L * >IN ! EXIT
*   THEN
*   SOURCE >IN ! DROP ; IMMEDIATE
* However since C/L (number of columns per line) is 64 (a power of 2), things
* can be coded in a more compact manner as: >IN @ 63 INVERT AND 64 + >IN !
BKSLSH	fcb	$81		ANSI (Block Ext)
	fcb	$5C		\ ( -- )
	fdb	SOURCE
	RFCS
	ldd	UBLK		BLK @ to D
	beq	@comser		We operate from block 0: the console
	ldd	UTOIN		>IN @
	andb	#^$3F		Point to the beginning of the line
	addb	#$40		next line
	adca	#0		Propagate potential carry from LSB
	std	UTOIN		>IN !
	rts			EXIT
@comser	RFXT	bsr,SOURCE+9	XT for SOURCE
	jsr	NPOP
	stx	UTOIN		>IN !
	RFXT	jmp,DROP+7	XT for DROP
	ENDC

PSTR	fcb	$82		ANSI (Core)
	fcc	'."'		Comp: ( "ccc<quote>" -- )
	fdb	BKSLSH		Exec: ( -- )
	RFCS
	RFXT	bsr,SQUOTE+5	XT for S"
	tst	USTATE+1
	bne	@pstcmp
	RFXT	jmp,TYPE+7	XT for TYPE
@pstcmp	RFXT	ldx,#TYPE+7	Emit TYPE as an XT
	jmp	EMXASXT

SQUOTE	fcb	$82		ANSI (Core)
	fcc	'S"'		Comp: ( "ccc<quote>" -- )
	fdb	PSTR		Exec: ( -- c-addr u )
	RFCS
	tst	USTATE+1
	bne	@sqcmp
	ldx	#'"		We are interpreting
	jsr	NPUSH
	RFXT	jsr,WORD+7	XT for WORD
	RFXT	jmp,COUNT+8	XT for COUNT
@sqcmp	ldy	DICEND		We are compiling
	lda	#JMPOPC		JMP extended
	sta	,y+
	pshs	y
	leay	2,y		2 ALLOT
	sty	DICEND
	ldx	#'"
	jsr	NPUSH
	RFXT	jsr,WORD+7	XT for WORD
	jsr	NPOP
	clra
	ldb	,x		C@
	leax	1,x		1+. Skip the byte count (X has HERE)
	leax	d,x		Skip string length material
	puls	y
	stx	,y		Install jump address
	leay	2,y		Counted string base address to Y
	lda	#LDXOPC
	sta	,x+
	sty	,x++
	stx	DICEND		Update HERE
	ldx	#NPUSH
	jsr	EMXASXT
	RFXT	ldx,#COUNT+8	XT for COUNT
	jmp	EMXASXT

* Transactional behaviour is guaranteed here. What this means is that the
* operation will preserve the data stack contents, should insufficient
* parameters be supplied. It is a very desirable feature (for debugging
* purposes) which will be generalized to a number of other words. As an
* aside, it also allows us to access the data stack as directly indexed
* through the 6309 U register, resulting in better performance.
DPLUS	fcb	2		79-STANDARD (REQ241)
	fcc	'D+'		( d1 d2 -- d1+d2--signed )
	fdb	SQUOTE		In processor's terms U has ( L1 H1 L2 H2)
	RFCS
	jsr	MIN4PST		Make sure we have at least 4 cells stacked up
* At this point sufficient stack depth has been assessed. Let's rock and roll!
	ldd	6,u		L1
	addd	2,u		L2
	std	6,u		d1+d2 least significant cell
	ldd	4,u		H1
	adcd	,u		H2 (add with carry bit)
	std	4,u		d1+d2 most significant cell
	leau	4,u		Drop 2 cells from the data stack
	rts

DNEG	fcb	7		79-STANDARD (REQ245)
	fcc	'DNEGATE'
	fdb	DPLUS
	RFCS
	jsr	MIN2PST		We need at least 2 cells stacked up
	ldw	2,u		Least significant cell
	comw
	ldd	,u		Most significant cell
	comd
	addw	#1
	adcd	#0		Propagate carry flag
	stq	,u		Store the result back to the data stack
	rts

DMINUS	fcb	2		79-STANDARD (double number extension)
	fcc	'D-'		( d1 d2 -- d1-d2--signed )
	fdb	DNEG		In processor's terms U has ( L1 H1 L2 H2)
	RFCS
	jsr	MIN4PST		Make sure we have at least 4 cells stacked up
	ldd	6,u		L1
	subd	2,u		L2
	std	6,u		d1-d2 least significant cell
	ldd	4,u		H1
	sbcd	,u		H2 (substract with borrow)
	std	4,u		d1-d2 most significant cell
	leau	4,u
	rts

DZEQ	fcb	3		79-STANDARD (double number extension)
	fcc	'D0='		( d -- flag )
	fdb	DMINUS
	RFCS
	RFXT	jsr,OR+5	XT for OR
	RFXT	jmp,NULP+5	XT for 0=

* GNU Forth has:
* f = FLAG(d1.hi==d2.hi ? d1.lo<d2.lo : d1.hi<d2.hi);
* Comparisons between high cells are signed, but they are unsigned between
* the low cells.
DLESS	fcb	2		79-STANDARD (REQ244)
	fcc	'D<'		( d1 d2 -- flag )
	fdb	DZEQ
	RFCS
	jsr	MIN4PST
* Data stack structure at this point:
* ,u	d2.hi			1 cell
* 2,u	d2.lo			1 cell
* 4,u	d1.hi			1 cell
* 6,u	d1.lo			1 cell
	clrf			A priori return value for FLAG
	ldd	,u		D2.HI
	cmpd	4,u		D1.HI
	bne	@term2
	ldd	2,u		D2.LO
	cmpd	6,u		D1.LO
	bls	@done
@anstru	decf			#$FF to F
@done	tfr	f,b
	sex			Sign extention B to D
	std	6,u		FLAG returned as a cell
	leau	6,u		Drop three cells from the data stack
	rts
@term2	ble	@done
	bra	@anstru

TWOOVER	fcb	5		ANSI (Core)
	fcc	'2OVER'		( d1 d2 -- d1 d2 d1 )
	fdb	DLESS
	RFCS
	jsr	MIN4PST		At least four cells need to be stacked up
	ldq	4,u		D:W has MSC:LSC of D1
	tfr	w,x
	jsr	NPUSH
	tfr	d,x
	jmp	NPUSH

TWOSWAP	fcb	5		ANSI (Core)
	fcc	'2SWAP'		( d1 d2 -- d2 d1 )
	fdb	TWOOVER
	RFCS
	jsr	MIN4PST		At least four cells must be stacked up
	ldx	4,u		D1 most significant cell
	ldy	6,u		D1 least significant cell
	ldq	,u		D:W has MSC:LSC of D2
	stq	4,u
	stx	,u
	sty	2,u
	rts

TWODROP	fcb	5		ANSI (Core)
	fcc	'2DROP'		( x1 x2 -- )
	fdb	TWOSWAP
	RFCS
	jsr	MIN2PST		At least two cells must be stacked up
	leau	4,u
	rts

TWODUP	fcb	4		ANSI (Core)
	fcc	'2DUP'		( double -- double double )
	fdb	TWODROP
	RFCS
	jsr	MIN2PST		At least two cells need to be stacked up
	ldq	,u		D:W has MSC:LSC of DOUBLE
	tfr	w,x
	jsr	NPUSH
	tfr	d,x
	jmp	NPUSH

TWOSTOR	fcb	2		ANSI (Core)
	fcc	'2!'		( x1 x2 a-addr -- )
	fdb	TWODUP
	RFCS
	jsr	MIN3PST		At least three cells need to be stacked up
	ldq	2,u		DOUBLE to D:W
	stq	[,u]		Store DOUBLE to ADDR
	leau	6,u		Drop three cells from the user stack
	rts

TWOFTCH	fcb	2		ANSI (Core)
	fcc	'2@'		( a-addr -- x1 x2 )
	fdb	TWOSTOR
	RFCS
	jsr	MIN1PST		At least cell needs to be stacked up
	ldq	[,u]		D:W has MSC:LSC of DBLADDR @
	stw	,u		Least significant cell stacked in place
	tfr	d,x		Most significant cell goes through standard push
	jmp	NPUSH

CONVERT	fcb	7		79-STANDARD (REQ195)
	fcc	'CONVERT'	( d1 addr1 -- d2 addr2 )
	fdb	TWOFTCH
	RFCS
	jsr	MIN3PST		At least 3 cells need to be stacked up
	jsr	CKBASE		Check for supported BASE. No return if not
	ldx	,u		ADDR1 to X
@cvloop	leax	1,x
	jsr	ISERCHD		End of input stream reached?
	beq	@cvoor		Yes--game over
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
        ENDC			CSSNTVE
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

CVTE	fcb	2		ANSI (Core)
	fcc	'#>'		( xd -- c-addr u )
	fdb	CONVERT
	RFCS
	jsr	NPOP
	jsr	NPOP		Drop 2 cells from the data stack
	ldx	#APADBUF
	UCNPUSH
	jsr	SLEN
	tfr	w,x
	UCNPUSH
	rts

SIGN	fcb	4		ANSI (Core)
	fcc	'SIGN'		( n -- )
	fdb	CVTE
	RFCS
	jsr	NPOP
	tfr	x,d
	tstd
	bge	@sign1
	ldb	#'-
	bra	PREAPAD
@sign1	rts

HOLD	fcb	4		ANSI (Core)
	fcc	'HOLD'		( char -- )
	fdb	SIGN
	RFCS
	jsr	NPOP
	tfr	x,d
	bra	PREAPAD		B is inserted at the beginning of APADBUF.

SHARPS	fcb	2		ANSI (Core)
	fcc	'#S'		( ud1 -- ud2 )
	fdb	HOLD
	RFCS
	lda	#1
	sta	CVTFCN		CVT function #1 is #S
	bra	CVT0

* Unsigned double on the top of the data stack gets divided by BASE.
* The division algorithm implemented here is the binary long division.
* See https://en.wikipedia.org/wiki/Division_algorithm for more information.
* Remainder (converted to a character) gets prepended to APAD.
CVT	fcb	1		ANSI (Core)
	fcc	'#'		( ud1 -- ud2 )
	fdb	SHARPS
	RFCS
	clr	CVTFCN		CVT function 0 is #
CVT0	jsr	NPOP
	tfr	x,y
	jsr	NPOP
* Stack structure:
* ,s	Remainder		16 bits
* 2,s	Bitmask high		16 bits
* 4,s	Bitmask low		16 bits
* 6,s	Quotient high		16 bits
* 8,s	Quotient low		16 bits
* 10,s	Numerator high		16 bits
* 12,s	Numerator low		16 bits
* 14,s	Denominator		16 bits
* 16,s	Remainder carry		8 bits
CVT1	leas	-17,s
	ldd	UBASE
	std	14,s		Denominator
	stx	12,s		Numerator least significant cell
	sty	10,s		Numerator most significant cell
	bsr	DBDIVSG
* Convert the remainder to a digit expressed in BASE.
	ldb	1,s
	lda	#'0
	cmpb	#10             B has the digit we want converted to BASE
	bcs	@cvtdgt
	lda	#'A-10
@cvtdgt	addr	a,b
	bsr	PREAPAD         Prepend B to the string currently in PAD
	tst	CVTFCN
	beq	@cvtend		Function 0 is straight #, i.e. we're done here
* Function 1 is #S, we iterate unless the quotient is 0.
	ldd	8,s
	ord	6,s
	beq	@cvtend
	ldx	8,s		New numerator low
	ldy	6,s		New numerator high
	leas	17,s		Discard the stack frame
	bra	CVT1		And go at it again
* Push back the quotient on the data stack (low then high cell).
@cvtend	ldx	8,s
	UCNPUSH
	ldx	6,s
	UCNPUSH
	leas	17,s		Discard the stack frame
	rts

* Insert the character in B in front of the string at APADBUF.
PREAPAD	ldx	#APADBUF
	jsr	SLEN
	addr	w,x		X points to the PAD string's NUL terminator
	incw			Include the terminator
	leay	1,x
	tfm	x-,y-
	stb	APADBUF
	rts

* Double (numerator high/low) gets divided by a single cell number. Please note
* that this is a strictly unsigned business. Input parameters are supposed to
* have been set up by the caller on the system stack. The quotient (high/low)
* and remainder are returned the same way. Current users of this routine are
* CVT (#) and UMSLMOD (UM/MOD).
*
* Stack structure:
* ,s	Return address		16 bits
* 2,s	Remainder		16 bits
* 4,s	Bitmask high		16 bits
* 6,s	Bitmask low		16 bits
* 8,s	Quotient high		16 bits
* 10,s	Quotient low		16 bits
* 12,s	Numerator high		16 bits
* 14,s	Numerator low		16 bits
* 16,s	Denominator		16 bits
* 18,s	Remainder carry		8 bits
*
DBDIVSG	ldd	#$8000
	std	4,s		Bitmask high
	clra
	std	6,s		Bitmask low
	std	8,s		Quotient high
	std	10,s		Quotient low
	std	2,s		Remainder
	ldf	#31		32 bits to go
@cvbeg	lsl	3,s
	rol	2,s		R := R << 1
	tfr	cc,a
	anda	#CFLAG
	sta	18,s		Save CFLAG after left shift of the remainder
* We need to extract bit <f> (i) of the numerator. Since (i) varies from 31
* downto 0, all we need to do is to shift left a 32 bit quantity and isolate
* the carry flag as the bit of interest.
	ldd	14,s		Numerator low
	lsld
	std	14,s
	ldd	12,s		Numerator high
	rold
	std	12,s
	tfr	cc,a
	anda	#CFLAG		CFLAG is 1, which is ideal here
	ora	3,s		R(0) := N(i)
	sta	3,s		Update the remainder
	tfr	a,b		Remainder LSB
	lda	2,s		Remainder MSB
	tst	18,s
	bne	@frcsub		Carry was set on left shift of the remainder
	cmpd	16,s		Denominator
	blo	@cvshft
@frcsub	subd	16,s
	std	2,s		R := R - D
* Q(i) := 1 (use the bitmask).
	ldd	8,s		Quotient high
	ord	4,s		Bitmask high
	std	8,s
	ldd	10,s		Quotient low
	ord	6,s		Bitmask low
	std	10,s
* Shift the bitmask 1 bit right.
@cvshft	ldd	4,s
	lsrd
	std	4,s
	ldd	6,s
	rord
	std	6,s
	decf
	bge	@cvbeg
	rts

CVTB	fcb	2		ANSI (Core)
	fcc	'<#'		( -- )
	fdb	CVT
	RFCS
	jsr	CKBASE		Sanity check. BASE can be altered at any time
	clr	APADBUF
	rts

DOT	fcb	1		ANSI (Core)
	fcc	'.'		( n -- )
	fdb	CVTB
	RFCS
	lda	#1
	sta	CVISSGN		Force a signed number conversion
PTOP0	jsr	NPOP		N to X
	jsr	CVNSTR
	ldx	#TBUFF
	lda	#SP
@ptop1	cmpa	,x+		Skip leading spaces
	beq	@ptop1
	leax	-1,x		Point to actual string start address
	jsr	PUTS
	jmp	PUTCH		Extra space after printing a number

UDOT	fcb	2		ANSI (Core)
	fcc	'U.'		( u -- )
	fdb	DOT
	RFCS
	clr	CVISSGN		Conversion is unsigned
	bra	PTOP0

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

BL	fcb	2		ANSI (Core)
	fcc	'BL'		( -- char )
	fdb	UDOTR
	RFCS
	ldx	#SP
	jmp	NPUSH

SPACE	fcb	5		ANSI (Core)
	fcc	'SPACE'		( -- )
	fdb	BL
	RFCS
	lda	#SP
	jmp	PUTCH

SPACES	fcb	6		ANSI (Core)
	fcc	'SPACES'	( n -- )
	fdb	SPACE
	RFCS
	jsr	NPOP
	lda	#SP
	tfr	x,w
	tstw
@loop	bne	@cont
	rts
@cont	jsr	PUTCH
	decw
	bra	@loop

PAGE	fcb	4		79-STANDARD (REF)
	fcc	'PAGE'
	fdb	SPACES
	RFCS
	ldx	#CSVT100
	jmp	PUTS

CRLF	fcb	2		ANSI (Core)
	fcc	'CR'		( -- )
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
	fcc	'TYPE'		( c-addr u -- )
	fdb	PAD
	RFCS
	jsr	NPOP		Character count (signed)
	tfr	x,w
	jsr	NPOP		Buffer address
	tstw			
@loop	bne	@cont
	rts
@cont	lda	,x+
	jsr	PUTCH
	decw
	bra	@loop

COUNT	fcb	5		ANSI (Core)
	fcc	'COUNT'		( c-addr1 -- c-addr2 u )
	fdb	TYPE
	RFCS
	jsr	NPOP		C-ADDR1 to X
	ldb	,x+		B has LSB(U) and X has C-ADDR2
	UCNPUSH			C-ADDR2 to the data stack (B is preserved)
	clra
	tfr	d,x
	jmp	NPUSH		U to the data stack

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

ACCEPT	fcb	6		ANSI (Core)
	fcc	'ACCEPT'	( c-addr +n1 -- +n2 )
	fdb	DASHTR
	RFCS
	jsr	NPOP
	tfr	x,d		Buffer length to B
	jsr	NPOP		Buffer address to X. B is preserved
	jsr	GETS		Input character count is returned via B
	clra
	pshu	d		This saves us "tfr d,x" and "UCNPUSH"
	rts

TERPRET	fcb	$49		79-STANDARD (REF) I make this compile time only
	fcc	'INTERPRET'	( -- )
	fdb	ACCEPT
	RFCS
* Obtain a base buffer address based on the value of BLK.
	ldx	UBLK
	bne	@isblk
* A Zero BLK value indicates the console OR a counted string set up by EVALUATE.
	ldx	#CMDBUF		Base buffer address for serial line input
	tst	SRCID		Counted string?
	beq	@rsolvd		No. Back to the serial console
	ldx	BSBFADR		BSBFADR and ISELEN have been set up previously
	bra	@rsolvd
* BLK is NZ, map the block in memory.
@isblk	jsr	NPUSH		X has the target block number
	RFXT	jsr,BLOCK+8	XT for BLOCK. Map the block in
	UCNPOP			Retrieve buffer address (to X)
* The physical address of the current block might have changed. This should be
* reflected by clearing ISEADDR.
	clrd
	std	ISEADDR
@rsolvd	stx	BSBFADR
* Note: >IN is supposed to have been set by the caller!
	ldd	UTOIN
	addr	d,x
	jmp	_INTERP		Finally invoke _INTERP.

LOAD	fcb	4		ANSI (Block)
	fcc	'LOAD'		( i*x u -- j*x )
	fdb	TERPRET
	RFCS
	jsr	NPOP		ZFLAG is set by NPOP
	bne	LOAD1
	rts			Block 0 is _not_ loadable
LOAD1	jsr	SAVINP		Save input parameters. X is preserved
	stx	UBLK		Update BLK with the LOAD argument
	ldd	#BLKSIZ		1024 bytes
	std	ISLEN		Set input stream length
	clrd
	sta	SRCID		Not invoked in EVALUATE context
LOAD2	std	UTOIN		Clear >IN
	std	ISEADDR		End of input stream address (included)
* Map the new BLK in, interpret code from there.
	RFXT	bsr,TERPRET+12	XT for INTERPRET
	jsr	RSTINP		Restore input parameters
	jmp	BKIN2PT		Map BLK in (if needed) and update BSBFADR

EVAL	fcb	8		ANSI (Core)
	fcc	'EVALUATE'	( i * x c-addr u -- j * x )
	fdb	LOAD
	RFCS
	jsr	MIN2PST		Need at least 2 parameters on the data stack
* The whole thing looks like LOAD1 except we do not need to map a block in.
	ldx	2,u		C-ADDR
	ldy	,u		U
	leau	4,u		Drop 2 cells from the data stack
	jsr	SAVINP		Save input context. X is preserved
	stx	BSBFADR		Set BSBFADR from C-ADDR
	sty	ISLEN		Set ISLEN from U
	lda	#$FF
	sta	SRCID		-1 (byte) to SRCID. Invoked in EVALUATE context
	clrd
	std	UBLK		Target block number is zero
	bra	LOAD2		Interpret, restore input context and proceed

THRU	fcb	4		ANSI (Block ext)
	fcc	'THRU'		( u1 u2 -- )
	fdb	EVAL
	RFCS
	jsr	NPOP
	tfr	x,y		Y has U2 (highblk)
	jsr	NPOP		X has U1 (lowblk)--both are unsigned numbers
@thrlop	cmpr	x,y
	bhs	@cont		Limit is >= to the loop index
	rts
@cont	pshs	x,y		Backup loop parameters
	UCNPUSH			Current block number to the data stack
	RFXT	bsr,LOAD+7	XT for LOAD
	puls	y,x		Retrieve loop parameters
	leax	1,x		Iterate over to the next screen
	bra	@thrlop

MILLIS	fcb	2		ANSI (Facility ext)
	fcc	'MS'		( u -- )
	fdb	THRU
	RFCS
	jsr	NPOP		ZFLAG is set by NPOP
	beq	@ms3
MILLIS1	ldd	#MSLCNT
@ms2	decd
	bne	@ms2
	leax	-1,x
	bne	MILLIS1
@ms3	rts

KEYP	fcb	4		ANSI (Facility)
	fcc	'KEY?'		( -- flag )
	fdb	MILLIS
	RFCS
	tfr	0,x
	tst	SERBCNT
	beq	@done
	leax	-1,x		Return the ANSI true
@done	jmp	NPUSH

KEY	fcb	3		ANSI (Core)
	fcc	'KEY'		( -- char )
	fdb	KEYP
	RFCS
	jsr	GETCH
	tfr	a,b
	clra
	tfr	d,x
	jmp	NPUSH

EMIT	fcb	4		ANSI (Core)
	fcc	'EMIT'		( x -- )
	fdb	KEY
	RFCS
	jsr	NPOP
	tfr	x,d
	tfr	b,a
	jmp	PUTCH

PLUS	fcb	1		ANSI (Core)
	fcc	'+'		( n1 | u1 n2 | u2 -- n3 | u3 )
	fdb	EMIT
	RFCS
	jsr	MIN2PST		We need at least two cells stacked up
	ldd	2,u		N1 to D
	addd	,u		D has N1+N2
	std	2,u		D to N3
	leau	2,u		Drop the top cell
	rts

ONEM	fcb	2		ANSI (Core)
	fcc	'1-'		( n1|u1 -- n2|u2 )
	fdb	PLUS
	RFCS
	jsr	MIN1PST		We need at least one cell stacked up
	ldd	,u
	decd
	std	,u
	rts

ONEP	fcb	2		ANSI (Core)
	fcc	'1+'		( n1|u1 -- n2|u2 )
	fdb	ONEM
	RFCS
	jsr	MIN1PST		We need at least one cell stacked up
	ldd	,u
	incd
	std	,u
	rts

CELLP	fcb	5		ANSI (Core)
	fcc	'CELL+'		( a-addr1 -- a-addr2 )
	fdb	ONEP
	RFCS
	jsr	MIN1PST		We need at least one cell stacked up
	ldd	,u
	addd	#2
	std	,u
	rts

MINUS	fcb	1		ANSI (Core)
	fcc	'-'		( n1|u1 n2|u2 -- n3|u3 )
	fdb	CELLP
	RFCS
	jsr	MIN2PST		We need at least two cells stacked up
	ldd	2,u		N1 to D
	subd	,u		D has N1-N2
	std	2,u		Store D to N3
	leau	2,u		Drop the top cell
	rts

FALSE	fcb	5		ANSI (Core ext)
	fcc	'FALSE'
	fdb	MINUS
	RFCS
	tfr	0,x
	jmp	NPUSH

TRUE	fcb	4		ANSI (Core ext)
	fcc	'TRUE'
	fdb	FALSE
	RFCS
	ldx	#-1
	jmp	NPUSH

SHIFT	fcb	5		79-STANDARD (REF)
	fcc	'SHIFT'
	fdb	TRUE
	RFCS
	jsr	MIN2PST		Two cells need to be stacked up
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
MULT	fcb	1		ANSI (Core)
	fcc	'*'		( n1|u1 n2|u2 -- n3|u3 [n4|u4] )
	fdb	SHIFT
	RFCS
	clr	MULFCN		Function 0 is *
MULT1	jsr	MIN2PST		Two cells need to be stacked up
	ldd	2,u		N1 to D
	muld	,u		D:W has N1*N2
	stw	2,u		LSC to N3
	tst	MULFCN
	bne	@mstsem
* Semantics: return only the LSC.
	leau	2,u		Drop one cell from the data stack
	rts
* M* semantics, also return the MSC.
@mstsem	std	,u		MSC to N4
	rts

MSTAR	fcb	2		ANSI (Core)
	fcc	'M*'		( n1 n2 -- d )
	fdb	MULT
	lda	#1
	sta	MULFCN		Function 1 is M*
	bra	MULT1

* The algorithm implemented here is similar to Donald's Knuth algorithm M,
* as described in "The Art of Computer Programming", Volume II, 3rd edition,
* section 4.3.1, pp 268. It is not as generic as algorithm M but fits our
* particular purpose here: a 16x16 bit unsigned multiplication that yields
* a 32 bit result. A simpler description of algorithm M can be found in
* Henry S. Warren Junior's "Hacker's Delight", second edition, section 8-1,
* pp 171. A C prototype implementation for a little endian host is provided
* in SW/util/umstar.c.
* In essence we reduce the problem to a 2 digit by 2 digit (expressed in
* base 256) multiplication and use the 6809 MUL (A * B -> D) instruction.
UMSTAR	fcb	3		ANSI (Core)
	fcc	'UM*'		( u1 u2 -- ud )
	fdb	MSTAR
	RFCS
	jsr	MIN2PST		2 parameters need to be stacked up
* Register allocation notes:
* X: pointer to ud1 (ARG_U/p in the C code).
* Y: pointer to ud2 (ARG_V/q in the C code).
* E: Inner loop index (i in the C code).
* F: Outer loop index (j in the C code).
*
* We save U to the system stack and use it to point to a scratch area allocated
* from the system stack that will contain the 4 bytes (double cell) ultimately
* holding the result (MSB first, what else?).
*
* System stack structure:
* ,S	Result scratch area (Result MSB)
* 2,S	Result scratch area (Result LSB)
* 4,S	Saved U register
* Total: 6 bytes.
	leas	-6,s		Allocate system stack scratch space
	stu	4,s		Save the U register
	clrd
	std	,s		Initialize the result's MSB
	std	2,s		Initialize the result's LSB
	leay	1,u		Initialize outer loop pointer (q in the C code)
	leau	3,s		Initialize the result pointer (r in the C code)
	ldf	#2		Outer loop index
@outer	ldx	4,s		Saved U register
	leax	3,x		Initialize inner loop pointer (p in the C code)
	lde	#2		Inner loop index
@inner	lda	,x		*p to A
	ldb	,y		*q to B
	mul			*p * *q to D (tmpval in the C code)
	addd	-1,u		r[-1] += tmpval
	std	-1,u
	bcc	@ncarry
	inc	-2,u		r[-2] += carry (propagate carry from ADDD above)
@ncarry leau	-1,u		r--
	leax	-1,x		p--
	dece			i--
	bne	@inner
	leau	1,u		r++
	leay	-1,y		q--
	decf			j--
	bne	@outer
	ldu	4,s		Restore the U register
	ldq	,s
	stq	,u		Result to the data stack (in place)
	leas	6,s		Release system stack scratch space
	rts

TWOTIM	fcb	2		ANSI (Core)
	fcc	'2*'		( x1 -- x2 )
	fdb	UMSTAR
	RFCS
	jsr	MIN1PST		One cell needs to be stacked up
	ldd	,u
	asld
	std	,u
	rts

TWODIV	fcb	2		ANSI (Core)
	fcc	'2/'		( x1 -- x2 )
	fdb	TWOTIM
	RFCS
	jsr	MIN1PST		One cell needs to be stacked up
	ldd	,u
	asrd
	std	,u
	rts

MOD	fcb	3		ANSI (Core)
	fcc	'MOD'		( N1 N2 -- N3 )
	fdb	TWODIV
	RFCS
	lda	#DVFMOD
	sta	DIVFCN		Function 1: return only the modulo
	bra	DIV1

SLMOD	fcb	4		ANSI (Core)
	fcc	'/MOD'		( N1 N2 -- N3 N4 )
	fdb	MOD
	RFCS
	clr	DIVFCN		Function 0: return the quotient and the modulo
	bra	DIV1

FMSLMOD	fcb	6		ANSI (Core)
	fcc	'FM/MOD'	( D1 N1 -- N2 N3 )
	fdb	SLMOD
	RFCS
	lda	#(DVFSLMD|DVOA1D) Function 0, arg #1 is double
	sta	DIVFCN
	bra	DIV1

SMSLREM	fcb	6		ANSI (Core)
	fcc	'SM/REM'	( D1 N1 -- N2 N3 )
	fdb	FMSLMOD
	RFCS
* Function 0, arg #1 is double, want symmetric division.
	lda	#(DVFSLMD|DVOA1D|DVOWSYM)
	sta	DIVFCN
	bra	DIV1

* /, MOD and /MOD are essentially the same function returning
* the different parts returned by DIVQ.
* We use a global variable to distinguish which functionality
* is being requested:
* 0: return the modulo and the quotient (/MOD).
* 1: return the modulo only (MOD).
* 2: return the quotient only (/).
DIV	fcb	1		ANSI (Core)
	fcc	'/'		( n1|d1 n2 -- n3 [n4] )
	fdb	SMSLREM
	RFCS
	lda	#DVFDIV
	sta	DIVFCN		Function 2: return only the quotient
DIV1	clr	F83DIVF		Assume no adjustment required for floored div.

* Split function code options into individual flags, extract base function code.
	clr	DIVDBL
	clr	DIVSYM
	lda	#1
	ldb	DIVFCN
	bitb	#DVOA1D
	beq	*+4
	sta	DIVDBL
	bitb	#DVOWSYM
	beq	*+4
	sta	DIVSYM
	andb	#DVFMASK
	stb	DIVFCN

	tst	DIVDBL
	bne	*+7		Make sure double stack requirements are met
	jsr	MIN2PST		At least two cells need to be stacked up
	bra	*+5
	jsr	MIN3PST		At least three cells need to be stacked up

	tst	DIVSYM
	bne	@dvsym		Symmetric division is wanted
	lda	2,u		Numerator's MSB
	eora	,u		Different sign from the denominator's MSB?
	bpl	@dvsym		No, proceed to the division code
	inc	F83DIVF		Numerator and denominator have different signs
@dvsym	tst	DIVDBL
	beq	@dvsgn
	ldq	2,u		Double cell numerator to D:W
	bra	@dvactu
* Division by zero conditions are dealt with through the trap handler.
@dvsgn	clrd			Clear the numerator's MSC
	ldw	2,u		Numerator's LSC
	bpl	@dvactu		Branch if no sign extention is needed
* Sign extention from W to Q.
	comd			-1 to D (numerator's MSC)
@dvactu	divq	,u		,u has the denominator
	bsr	FDIVADJ		Perform floored division adjustment, if needed

	tst	DIVDBL
	beq	*+4
	leau	2,u		Drop one cell from the data stack

	tst	DIVFCN
	bne	@div4
	std	2,u		Function 0: return the modulo and the quotient
@div3	stw	,u		Function 2: return only the quotient
	rts
@div4	leau	2,u		Drop one cell from the data stack
	tfr	d,v		Backup the modulo
	lda	DIVFCN
	cmpa	#1
	bne	@div3
	tfr	v,d		Restore the modulo
	std	,u		Function 1: return only the modulo
	rts

* Quotient is returned in W, modulo in D but this is symmetric division :-(
* Need to return a floored division result for compat. with F83 and ANSI impls.
FDIVADJ	tstd			Is the remainder zero?
	beq	@no83ad		Yes. No adjusment required for floored division
	tst	F83DIVF		Different signs for numerator and denominator?
	beq	@no83ad		No. Adjusment not needed for floored division
* Adjusment for F83 floored division.
	decw			Decrement the quotient
	addd	,u		Add the denominator to the modulo
@no83ad	rts

STRSLSH	fcb	2		ANSI (Core)
	fcc	'*/'		( N1 N2 N3 -- N4 [N5] )
	fdb	DIV
	RFCS
	lda	#1
	sta	STSLFCN
STRSL1	jsr	MIN3PST		Three cells need to be stacked up
	clr	F83DIVF		Assume no adjustment required for floored div.
* Division by zero conditions are dealt with through the trap handler.
	ldd	4,u		N1
	muld	2,u		N2 (N1 * N2 -> D:W)
	pshs	a		Product's MSC's MSB
	eora	,u		Different sign from the denominator's?
	bpl	@stslpr		No. Floored division adjustment not needed
	inc	F83DIVF
@stslpr	puls	a
	divq	,u		N3
	bsr	FDIVADJ		Perform floored division adjustment, if needed
	leau	2,u
	tst	STSLFCN
	bne	@strsl2		Just the quotient, Ma'am!
	std	2,u		N4: the modulo
	stw	,u		N5: the quotient
	rts
@strsl2	leau	2,u
	stw	,u		N4
	rts

STRSLMD	fcb	5		ANSI (Core)
	fcc	'*/MOD'		( N1 N2 N3 -- N4 N5 )
	fdb	STRSLSH
	RFCS
	clr	STSLFCN
	bra	STRSL1

UMSLMOD	fcb	6		ANSI (Core)
	fcc	'UM/MOD'	( ud u1 -- u2 u3 )
	fdb	STRSLMD
	RFCS
	jsr	MIN3PST
	ldd	,u		Is U1 zero?
	bne	@cont		No
@oor	ldb	#4		Division by zero/Out of range
	jsr	ERRHDLR		No return
@cont	jsr	NPOP
	tfr	x,d		U1 (denominator) to D
	jsr	NPOP
	tfr	x,y		Numerator MSC TO Y
	jsr	NPOP		Numerator LSC to X
	leas	-17,s
	std	14,s		Denominator
	stx	12,s		Numerator least significant cell
	sty	10,s		Numerator most significant cell
	jsr	DBDIVSG
	ldd	6,s		Quotient high
	bne	@oor		Out of range
	ldx	,s		Remainder (U2)
	UCNPUSH
	ldx	8,s		Quotient low (U3)
	UCNPUSH
	leas	17,s
	rts

* Returns the current value of the Sreg register (informational only).
* This word is either called (JSROPC) or jumped to (JMPOPC), as a result
* of the tail call optimization process. There is no way to tell the
* difference. Here we assume that it is called and return Sreg.
SYSSTK	fcb	1		Non-standard
	fcc	'S'
	fdb	UMSLMOD
	RFCS
	tfr	s,x
	jmp	NPUSH

SYSSTAT	fcb	2
	fcc	'S@'
	fdb	SYSSTK
	RFCS
	ldx	,s
	jmp	NPUSH

PAYLOAD	fcb	7		Non standard
	fcc	'PAYLOAD'	( -- len ) where len is the code payload
	fdb	SYSSTAT		of the word located by FIND (or NULL)
	RFCS
	ldx	FNDPLD		Code payload reported by FIND
	jmp	NPUSH

* Differences from the original code (WORDS):
* - display number in HEX rather than in the current base.
* - dropped feat: the original stuff was interactively paged by 15 line screens.
* - added feat: display code implementation payload.
* - added feat: display the immedediate and define (compile time only) flags.
* - added feat: display the forgettable status (R/W). Everything user
*   defined is forgettable (i.e. RAM resident).
WORDS	fcb	5		Non-standard
	fcc	'WORDS'		( -- )
	fdb	PAYLOAD
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
	ENDC			RELFEAT
	tfr	x,d		CVNSTR depends on UBASE and we want HEX output
	ldy	#TBUFF
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
	ENDC			RELFEAT
	subr	y,x
	tfr	x,d
	ldy	#TBUFF
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
	ENDC			RELFEAT
* Check for forgettable also. In ROM => R else W unless we just processed '@'.
	lda	#'R
	ldy	,s
	IFNE	RELFEAT
	leay	3,y		Word entry point to Y
	ELSE
	leay	2,y		Word entry point to Y
	ENDC			RELFEAT
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

STATE	fcb	5		ANSI (Core)
	fcc	'STATE'		( -- a-addr )
	fdb	WORDS
	RFCS
	ldx	#USTATE
	jmp	NPUSH

BASE	fcb	4		ANSI (Core)
	fcc	'BASE'		( -- a-addr )
	fdb	STATE
	RFCS
	ldx	#UBASE
	jmp	NPUSH

DECIMAL	fcb	7		ANSI (Core)
	fcc	'DECIMAL'	( -- )
	fdb	BASE
	RFCS
	ldd	#10
BASESET	std	UBASE
	rts

HEX	fcb	3		ANSI (Core)
	fcc	'HEX'		( -- )
	fdb	DECIMAL
	RFCS
	ldd	#16
	bra	BASESET

DOTTICK	fcb	2		Non-standard (SwiftForth)
	fcb	$2E,$27		.' ( memaddr -- )
	fdb	HEX
	RFCS
	IFNE	SSDFEAT
	jsr	NPOP
	tfr	x,y
	ldx	#HEXBUF
	jsr	FINDSYM
	ELSE
	lda	#'$
	jsr	PUTCH
	jsr	NPOP
	tfr	x,d
	ldy	#HEXBUF
	jsr	HDMP4
	ENDC			SSDFEAT
	ldx	#HEXBUF
	jmp	PUTS

* Display a dump of the data stack in the current BASE. In Leo Brodie's
* "Starting Forth" the data stack is printed from the bottom up. So it is here.
DDUMP	fcb	2		ANSI (Optional "Programming tools" word set)
	fcc	'.S'		( -- )
	fdb	DOTTICK
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

QRYDUP	fcb	4		ANSI (Core)
	fcc	'?DUP'		( x -- 0 | x x )
	fdb	DDUMP
	RFCS
	jsr	NPOP		ZFLAG is set by NPOP
	UCNPUSH			Push back the original parameter
	lbne	NPUSH		And DUP if NZ
	rts

TUCK	fcb	4		ANSI (Core ext)
	fcc	'TUCK'		( x1 x2 -- x2 x1 x2 ) i.e. SWAP OVER
	fdb	QRYDUP
	RFCS
	RFXT	bsr,SWAP+7	XT for SWAP
	RFXT	bra,OVER+7	XT for OVER

NIP	fcb	3		ANSI (Core ext)
	fcc	'NIP'		( x1 x2 -- x2 ) i.e. SWAP DROP
	fdb	TUCK
	RFCS
	RFXT	bsr,SWAP+7	XT for SWAP
	RFXT	bra,DROP+7	XT for DROP

DUP	fcb	3		ANSI (Core)
	fcc	'DUP'		( x -- x x )
	fdb	NIP
	RFCS
	jsr	MIN1PST		At least one cell needs to be stacked up
	ldx	,u
	jmp	NPUSH

DROP	fcb	4		ANSI (Core)
	fcc	'DROP'		( x -- )
	fdb	DUP
	RFCS
	jmp	NPOP

SWAP	fcb	4		ANSI (Core)
	fcc	'SWAP'		( x1 x2 -- x2 x1 )
	fdb	DROP
	RFCS
	jsr	MIN2PST		We need at least two cells stacked up
	ldq	,u		In place SWAP
	exg	d,w
	stq	,u
	rts

PICK	fcb	4		ANSI (Core ext)
	fcc	'PICK'		( xu ... x1 x0 u -- xu ... x1 x0 xu )
	fdb	SWAP
	RFCS
	jsr	NPOP		Arg <u> to X (expressed in cells)
PICK1	ldd	#NSTBOT
	subr	u,d
	lsrd			D has the data stack depth in cells
	cmpr	d,x		We need to make sure (unsigned) X < D
	bhs	@pick1
@pick1	tfr	x,d
	lsld			Arg <u> cells byte count to D
	leax	d,u
	tfr	x,y		For the sake of ROLL's implementation
	ldx	,x
	UCNPUSH
	rts
	ldb	#13		Argument is greater than or equal to DEPTH
	jsr	ERRHDLR		No return

OVER	fcb	4		ANSI (Core)
	fcc	'OVER'		( x1 x2 -- x1 x2 x1 )
	fdb	PICK
	RFCS
	ldx	#1
	bra	PICK1

ROLL	fcb	4		ANSI (Core ext)
	fcc	'ROLL'		( xu xu-1 ... x0 u -- xu-1 ... x0 xu )
	fdb	OVER
	RFCS
	jsr	NPOP
ROLL1	tfr	x,w		Backup arg <u> to W
	bsr	PICK1		Let PICK do the error handling
	leay	1,y		Point to the LSB of the cell being picked
	leax	-2,y
	incw
	addr	w,w
	tfm	x-,y-
	jmp	NPOP

ROT	fcb	3		ANSI (Core)
	fcc	'ROT'		( x1 x2 x3 -- x2 x3 x1 )
	fdb	ROLL
	RFCS
	ldx	#2
	bra	ROLL1

MROT	fcb	4
	fcc	'-ROT'
	fdb	ROT
	RFCS
	RFXT	bsr,ROT+6	XT for ROT
	RFXT	bra,ROT+6	XT for ROT

CCOMMA	fcb	2		ANSI (Core)
	fcc	'C,'		( char -- )
	fdb	MROT
	RFCS
	jsr	NPOP
	tfr	x,d
	ldy	DICEND
	stb	,y+
	sty	DICEND
	rts

COMMA	fcb	1		ANSI (Core)
	fcc	','		( x -- )
	fdb	CCOMMA
	RFCS
	jsr	NPOP
	ldy	DICEND
	stx	,y++
	sty	DICEND
	rts

ALLOT	fcb	5		ANSI (Core)
	fcc	'ALLOT'		( n -- )
	fdb	COMMA
	RFCS
	jsr	NPOP		N to X
	ldd	DICEND
	leax	d,x
	stx	DICEND		Adjust HERE
	rts

FILL	fcb	4		ANSI (Core)
	fcc	'FILL'		( c-addr u char -- )
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

BLANK	fcb	5		ANSI (String)
	fcc	'BLANK'		( c-addr u -- )
	fdb	FILL
	RFCS
	ldw	#SP
	bra	FILL1

CMOVED	fcb	6		FORTH-83
	fcc	'CMOVE>'
	fdb	BLANK
	RFCS
	jsr	ACQMOVP
	tstw
	beq	@cmovd1
CMOVD1	decw
	addr	w,x
	addr	w,y
	incw
	tfm	x-,y-
@cmovd1	rts

CMOVE	fcb	5		79-STANDARD (REQ153)
	fcc	'CMOVE'
	fdb	CMOVED
	RFCS
	jsr	ACQMOVP
	tfm	x+,y+
	rts

* Functionally:
* : MOVE ( addr1 addr2 u -- )      \ u is expressed in bytes
*   DUP 2OVER                      \ addr1 addr2 u u addr1 addr2
*   SWAP -                         \ addr1 addr2 u u addr2-addr1
*   SWAP                           \ addr1 addr2 u addr2-addr1 u
*   U< IF CMOVE> ELSE CMOVE THEN ;
* For those who care to read the 1994 specification, arg <u> is expressed in
* address units. For the rest of us, mere mortals, this is just a byte count.
* As an aside, this is functionality provided by the glibc memmove function.
MOVE	fcb	4		ANSI (Core)
	fcc	'MOVE'		( addr1 addr2 u -- )
	fdb	CMOVE
	RFCS
	jsr	ACQMOVP		ADDR1 -> X, ADDR2 -> Y, U -> W
	tstw
	beq	@movend
* addr2 addr1 - u U< IF CMOVE> ELSE CMOVE THEN
	tfr	y,d		D has addr2 (dest)
	subr	x,d		D has addr2 - addr1 (dest - src)
	cmpr	w,d
	blo	CMOVD1		CMOVE>
	tfm	x+,y+		CMOVE
@movend	rts

CELLS	fcb	5		ANSI (Core)
	fcc	'CELLS'		( n1 -- n2 )
	fdb	MOVE
	RFCS
	jsr	NPOP		N1 to X
	addr	x,x		Times 2
	UCNPUSH			X to N2
	rts

LAST	fcb	4		79-STANDARD (REF)
	fcc	'LAST'
	fdb	CELLS
	RFCS
	ldx	LSTWAD
	jmp	NPUSH

HERE	fcb	4		ANSI (Core)
	fcc	'HERE'		( -- addr )
	fdb	LAST
	RFCS
	ldx	DICEND
	jmp	NPUSH

PLUSST	fcb	2		ANSI (Core)
	fcc	'+!'		( n|u a-addr -- )
	fdb	HERE
	RFCS
	jsr	MIN2PST		We need at least two cells stacked up
	ldx	,u		ADDR to X
	ldd	,x		@ADDR to D
	addd	2,u		Add INCR to D
	std	,x		Store the sum back to ADDR
	leau	4,u		Drop two cells from the data stack
	rts

CSTORE	fcb	2		ANSI (Core)
	fcc	'C!'		( char c-addr -- )
	fdb	PLUSST
	RFCS
	jsr	MIN2PST		We need at least two cells stacked up
	lda	3,u		CHAR to A
	sta	[,u]		Actual store to C-ADDR
	leau	4,u		Drop two cells from the data stack
	rts

STORE	fcb	1		ANSI (Core)
	fcc	'!'		( x a-addr -- )
	fdb	CSTORE
	RFCS
	jsr	MIN2PST		At least two cells need to be stacked up
	ldd	2,u		X to D
	std	[,u]		Actual store to A-ADDR
	leau	4,u		Drop two cells from the user stack
	rts

CFETCH	fcb	2		ANSI (Core)
	fcc	'C@'		( c-addr -- char )
	fdb	STORE
	RFCS
	jsr	MIN1PST		We need at least one cell stacked up
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
FETCH	fcb	1		ANSI (Core)
	fcc	'@'		( a-addr -- x )
	fdb	QMARK
	RFCS
	jsr	MIN1PST		At least one cell needs to be stacked up
	ldd	[,u]
	std	,u		Returned through the data stack
	rts

REALEND	equ	*

*******************************************************************************
* String literals.

* Using CR+LF as it is Minicom's default.

* Clear the screen, VT100 style.
CSVT100	fcb	$1B,'[','H',$1B,'[','J',CR,NUL

BOOTMSG	fcb	CR,LF
	IFNE	RTCFEAT
	fcc	'Z79Forth/AR 6309 ANS Forth System'
	ELSE
	fcc	'Z79Forth/A  6309 ANS Forth System'
	ENDC			RTCFEAT
	fcb	CR,LF
	fcc	'20221017 (C) Francois Laagel 2019'
	fcb	CR,LF,CR,LF,NUL

RAMOKM	fcc	'RAM OK: 32 KB'
CRLFSTR	fcb     CR,LF,NUL

RAMFM	fcc	'RAM check failed'
	fcb     CR,LF,NUL

	IFNE	RTCFEAT
RTPRESM	fcc	'MC146818 RTC'
	fcb	CR,LF,NUL
	ENDC

	IFEQ	CSSNTVE
OKFEEDB	fcc	' ok'		As per GNU Forth's implementation...
	ELSE
OKFEEDB	fcc	' OK'
	ENDC			CSSNTVE
	fcb	CR,LF,NUL

* Error messages for IODZHDL.
IOPERRM	fcn	'Illegal opcode near '
DV0ERRM	fcn	'Division by 0 near '

ERRMTBL	fcn	'Data stack overflow'	Error 0
	fcn	'Data stack underflow'	Error 1
	fcn	'?'			Error 2
	fcn	'User ABORT'		Error 3
	fcn	'OoR error'		Error 4 (formerly 'Division by zero')
	fcn	'Missing word name'	Error 5
	fcn	'Incorrect STATE'	Error 6
	fcn	'Return stack overflow'	Error 7
	fcn	'Return stack underflow' Error 8
	fcn	'Illegal construct'	Error 9
	fcn	'Assertion failed'	Error 10
	fcn	'RO word'		Error 11
	fcn	'Missing delimiter'	Error 12
	fcn	'Illegal argument'	Error 13
	fcn	'No matching CREATE'	Error 14
	fcn	'Invalid BASE'		Error 15
	fcn	'Word name too long'	Error 16
	fcn	'IO error'		Error 17

* A-list used for numeric literal base prefixes.
BASALST	fcc	'$'		Hexadecimal prefix
	fcb	16
	fcc	'&'		Decimal prefix (as in LWASM, VolksForth)
	fcb	10
	fcc	'#'		Decimal prefix
	fcb	10
	fcc	'%'		Binary prefix
	fcb	2
	fcc	'@'		Octal prefix
	fcb	8
	fcb	0		End of list marker

* Under no circumstance should the following symbol be negative!
AVL	equ	VECTBL-*	Available EEPROM space left

*******************************************************************************
* Interrupt vector table
	org	VECTBL

	fdb	IODZHDL		Illegal opcode/Division by zero trap
	fdb	SWI3HDL		SWI 3 interrupt vector address
	fdb	SWI2HDL		SWI 2 interrupt vector address
	fdb	FIRQHDL		FIRQ interrupt vector address
	fdb	IRQHDL		IRQ interrupt vector address
	fdb	SWIHDL		SWI interrupt vector address
	fdb	NMIHDL		NMI vector address
	fdb	RSTHDL		RESET vector address


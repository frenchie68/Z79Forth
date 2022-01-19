	IFNE RTCFEAT

* FIRQ handler for the MC146818 RTC.
* In this particular context, only D and CC might be altered.
RTCIHDL	tst	RTCAVL		RTC chip detected?
	beq	@notick		Nope
	lda	#RTOREGC	Read RTCC. INTACK if any interrupt is pending.
* Two intructions inlined for RTREGRD in non-preemtible (interrupt) mode.
	sta	RTAS
	ldb	RTDS
	andb	#RTCPF		RTC periodic interrupt pending?
	beq	@notick		No, this ain't us ticking
* From "The 6309 Book" p. 3-69: "Note that INC does not effect the carry bit."
@inctks	ldd	TIKSLOW
	addd	#1
	std	TIKSLOW
	ldd	TIKSHI
	adcd	#0
	std	TIKSHI
@notick	rts

* Read one byte from an internal's RTC register whose offset is in A upon
* routine entry. The register contents is returned in B.
* No other register is altered. FIRQ is temporarily disabled.
* Can be called from base or interrupt level.
RTREGRD	pshs	cc
	orcc	#FFLAG		Mask FIRQ
	sta	RTAS		Select target RTC register
	ldb	RTDS		Read RTC register contents
	puls	cc		Restore previous interrupt handling mode
	rts

* Write one byte to an internal's RTC register whose offset is in A upon
* routine entry. The register byte output value is in B upon entry.
* FIRQ is temporarily masked. All regs contents are preserved.
* Can be called from base or interrupt level.
RTREGWR	pshs	cc
	orcc	#FFLAG		Mask FIRQ
	sta	RTAS		Select target RTC register
	stb	RTDS		Write RTC register contents
	puls	cc		Restore previous interrupt handling mode
	rts

* Detect if an MC146818 chip is present.
RTCINIT
	IFNE	DEBUG
	clrd
	std	TIKSHI		Initialize the TICKS double cell
	std	TIKSLOW
	clr	RTCAVL		Assume chip not present
	ENDC
	lda	#RTOPRES	Offset to the last NVRAM byte if dev is present
	bsr	RTREGRD		Read user memory byte. Value is returned in B	
	comb			One's complement to B
	pshs	b
	bsr	RTREGWR		Write back one's complement to RTC
	bsr	RTREGRD		And read the last NVRAM byte back
	puls	a
	cmpr	b,a		Match?
	beq	@rtdtct		RTC chip detected all right!
	rts
* MC146818 RTC chip detected.
* On power up if register RTCA reads as 0, the SET bit in register RTCB will
* prevent any updates to the calendar (date/time). We do preserve the state of
* the SET bit, so as to indicate to the application software that the time and
* date have yet to be set manually.
@rtdtct	lda	#RTOREGA
	ldb	#(RTADV32|RTARS64)
	bsr	RTREGWR		Write B to the RTCA register
	lda	#RTOREGB
	bsr	RTREGRD
	andb	#RTBSET		Extract the SET bit
	orb	#(RTBPIE|RTBDM|RTB24)
	bsr	RTREGWR		Write B to the RTCB register
	inc	RTCAVL
	ldx	#RTPRESM	Real time clock detected message base address
	jmp	PUTS		Out to the console

	ENDC			RTCFEAT


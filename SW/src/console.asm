* FIRQ interrupt handler. This is entered on RDRF (input available).
FIRQHDL	pshs	x,d
	lda	ACIACTL
	anda	#ACIISVC	Does the ACIA need input service?
	beq	@nsint		No. This is not the IRQ source we're looking for
	ldb	ACIADAT		Incoming data byte to B (INTACK)
	cmpb	#ETX		Control-C?
	beq	@sigint		Yes
	cmpb	#XOFF
	beq	@outngo		Output is a no go
	cmpb	#XON
	beq	@outok		Output is re-enabled
	lda	SERBCNT
	cmpa	#SERBSZ
	beq	@nsint		Serial input buffer is full. So it goes...
	ldx	#SERBUF
	lda	SERBENQ		Enqueue offset to A
	stb	a,x		Enqueue incoming character
	inca
	anda	#SERBSZ-1	Modulo arithmetic
	sta	SERBENQ
	lda	SERBCNT
	inca
	cmpa	#SERBSZ-32	Serial input buffer considered full?
	bne	@upsbcn		No
	ldb	#ACIRTS1
	stb	ACIACTL		Set RTS# to high
@upsbcn	sta	SERBCNT		Update serial buffer byte count
@nsint	puls	d,x
	rti
* Control-C was recognized.
@sigint	leas	4,s		Drop D and X
	lda	SERBENQ
	sta	SERBDEQ
	clr	SERBCNT		Serial input buffer has been emptied
	RFXT	jsr,NCLR+7	Clear the data stack
	RFXT	jsr,RCLR+7	and the return stack
	ldy	1,s		Saved PC from the FIRQ stack
	ldx	#ERRHD1
	stx	1,s		Execution continues in the error handler
	ldb	#3		with ABORT error code passed through B
	rti
@outngo	clra
	bra	@sxmsta
@outok	lda	#1
@sxmsta	sta	XMITOK		Update XMIT status flag
	bra	@nsint

* We do not have to talk to the ACIA directly, unless SERBCNT is zero,
* in which case we have to lower RTS#, so as to accept incoming characters.
GETCH	pshs	x,d
	orcc	#FFLAG		Disable FIRQ
	tst	SERBCNT
	bne	@sbdind		Some data is available
	lda	#ACIRTS0
	sta	ACIACTL		Clear RTS#
	andcc	#^FFLAG		Enable FIRQ
@again	sync			Go to sleep and resume on interrupt
SYNCRA	tst	SERBCNT
	beq	@again
* Serial buffer data indication.
@sbdind	andcc	#^FFLAG		Re-enable FIRQ
	ldx	#SERBUF
	lda	SERBDEQ		Dequeue offset to A
	ldb	a,x		Buffered input character to B
	stb	,s		Incoming character to A in the caller's stack
	inca
	anda	#SERBSZ-1	Modulo arithmetic
	sta	SERBDEQ
	dec	SERBCNT
	puls	d,x
	rts

PUTCH	pshs	b
	ldb	#ACITDRE
@tdrdrn	bitb	ACIACTL
	beq	@tdrdrn		Drain transmit data register
@wfxon	tst	XMITOK		Software flow control on output
	beq	@wfxon		Wait for XON
	sta	ACIADAT         Transmit data
	puls	b
	rts

* Send NUL terminated string pointed to by X to the ACIA.
PUTS	pshs	x,d
@puts1	lda	,x+
	beq	@puts2         NUL marks the end of the string
	bsr	PUTCH
	bra	@puts1
@puts2	puls	d,x
	rts

PUTCR	pshs	x
	ldx	#CRLFSTR
	bsr	PUTS
	puls	x
	rts

_BS	lda	#BS
	bsr	PUTCH          Output BS
	lda	#SP
	bsr	PUTCH          Clear character
	lda	#BS
	bra	PUTCH          And go back again

* Receive CR terminated string and store it to X.
* Upon entry B contains the receiving buffer length.
* (excluding the NUL terminator). On exit, B will
* contain the number of characters entered (excluding
* the trailing NUL). A and X are preserved. B will
* have the actual number of characters entered.
* Implementation of $05D9 in the TRS-80 Level II ROM.
GETS	cmpb	#2
	bhs	@gets0		B must be 2 or more
	rts
@gets0	pshs	x,d
@gets1	bsr	GETCH
	cmpa	#BS		Backspace?
	bne	@gets3
	cmpb	1,s		B upon routine entry
	beq	@gets1		Do not go beyond the beginning of the buffer
	bsr	_BS
	leax	-1,x
	incb
	bra	@gets1
@gets3	cmpa	#NAK		Kill input?
	bne	@gets5
* While B != 1,S (initial buffer length), decrement X, increment B.
@gets4	cmpb	1,s
	beq	@gets1
	bsr	_BS
	leax	 -1,x
	incb
	bra	@gets4
* Regular input handling: echo input character.
@gets5	cmpa	#CR
	beq	@gets6		Minicom only sends CR
	bsr	PUTCH
	sta	,x+
	decb
	cmpb	#1		End of buffer reached?
	bne	@gets1
@gets6	lda	#SP
	bsr	PUTCH
	clr	,x
	tfr	x,d
	subd	2,s		Actual number of characters entered
	stb	1,s		Stored to B (through the system stack).		
	puls	d,x
	rts


PUTCH	pshs	b
	ldb	#ACITDRE
@ptch1	bitb	ACIACTL         Wait for TDRE bit to be set
	beq	@ptch1
	sta	ACIADAT         Transmit data
	puls	b
	rts

GETCH	lda	#ACIRTS0
	sta	ACIACTL		You may talk to me now
	lda	#ACIRDRF
@again	bita	ACIACTL
	beq	@again
	lda	#ACIRTS1
	sta	ACIACTL		You may shut up now
@getdat	lda	ACIADAT		Get character from the ACIA
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


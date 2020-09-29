* Wait until the BSY bit is clear in the CF status register.
* Upon return the BSY bit is guaranteed to be zero. Z will
* be set in CC. This subroutine should not be called before
* the CompactFlash presence has been assessed.
CFWAIT  lda     CFSTATR
        bita    #CFBSYB
        bne     CFWAIT
        rts

* Wait until the BSY bit is clear and the RDY bit is set.
CFRDY	bsr	CFWAIT
	beq	@cfrdy0		Card not busy, proceed
	orcc	#ZFLAG
	rts
@cfrdy0	ldx	#$2000		Maximum poll count
	lda	#CFRDYB
@cfrdy1	bita	CFSTATR
	bne	@cfrdy2		BSY bit is set, continue
	leax	-1,x		Card not ready, decrement poll count
	bne	@cfrdy1		and try again
@cfrdy2	rts			Return with Z set: card not present
*				Return with Z clear: card is ready

* Check for possible error condition.
* Upon return either Z is set (A is zero) and no error occurred or
* Z is clear and the CF error register contents is returned in A.
CFERRCK	bsr	CFWAIT
	lda	CFSTATR
	anda	#CFERRB
	beq	@cfeck1
	lda	CFERROR		The error register contains error details
@cfeck1	rts

* Issue CF command. The command code is passed through the B register.
* Upon return:
* - Z is set and everything is peachy.
* - or Z is clear and the CF error register contents will be
*   returned in A (and mirrored in the global CFERRCD variable).
* B is preserved.
CFCMDIS	bsr	CFRDY
	beq	@cfciab		Card not ready, abort
	stb	CFCMMIR		Update CF command mirror
	stb	CFCOMDR		Send command to the CF card
	bsr	CFERRCK
@cfcise	sta	CFERRCD		CF command issue set error
	IFNE	DEBUG
	pshs	a,cc
	beq	@cfciok
	lda	#'!
	jsr	PUTCH
@cfcidi	lda	CFCMMIR
	ldx	#TBUFF
	jsr	HDMP2
	lda	#'/
	sta	,x+
	lda	CFERRCD
	jsr	HDMP2
	ldx	#TBUFF
	jsr	PUTS
	jsr	PUTCR
	bra	@cfcict		CF command issue continued
@cfciok	lda	#'#
	jsr	PUTCH
	bra	@cfcidi		CF command issue data indication
@cfcict	puls	cc,a
	ENDC
	rts
@cfciab	lda	#$FF		Dummy error code
	bra	@cfcise

* Drive 0 select.
CFDRSEL	bsr	CFRDY
	lda	#$E0		Drive 0, LBA mode
	sta	CFDRHDR
	bra	CFRDY

* Analyze the response to the CF "Identify Device" request.
CFANTHS	ldx	DICEND
	leax	$36,x		Point to the "Model number" field
	pshs	x
	lde	#$14		Number of 16 bit words to go through
@bytswp	ldd	,x
	exg	b,a		Byte swapping
	std	,x++
	dece
	bne	@bytswp
	clr	,x
	puls	x
	jsr	PUTS		Decent CF cards indicate the capacity that way
	jmp	PUTCR		Up to $10000 blocks is 64 MB!

* Initialize the CompactFlash subsystem.
* This is run once at boot time. There is no hot plug support here.
CFINIT	clr	CFCARDP		Clear card present,
	IFEQ	DEBUG
	bsr	CFRDY
	ELSE
	jsr	CFRDY
	ENDC
	beq	@cfinab		Card not ready, abort
	bsr	CFDRSEL		Drive 0 select
	lda	#1		Enable 8-bit data transfers
	sta	CFFEATR
	ldb	#CFSETFT	Issue a CF "Set Feature" command
	IFEQ	DEBUG
	bsr	CFCMDIS		Not allowed to fail
	ELSE
	jsr	CFCMDIS		Not allowed to fail
	ENDC
	bne	@cfinab		Abort with card present flag clear
	lda	#1		Request default PIO mode wo/ IORDY
	sta	CFSCNTR
	lda	#3		ATA-2 "Set Transfer mode"
	sta	CFFEATR
	IFEQ	DEBUG
	bsr	CFCMDIS		OK to fail
	ELSE
	jsr	CFCMDIS		OK to fail
	ENDC
	lda	#$82		Disable write caching
	sta	CFFEATR
	IFEQ	DEBUG
	bsr	CFCMDIS		B still has #CFSETF. Also OK to fail
	ELSE
	jsr	CFCMDIS		B still has #CFSETF. Also OK to fail
	ENDC
	ldb	#CFIDDEV	Issue a CF "Identify Device" command
	IFEQ	DEBUG
	bsr	CFCMDIS		Not allowed to fail
	ELSE
	jsr	CFCMDIS		Not allowed to fail
	ENDC
	bne	@cfinab		Abort with card present flag clear
	ldx	DICEND		Target address is HERE
	bsr	CF1SRD		Read one sector
	bsr	CFANTHS		Analyze this!
	lda	#1
	sta	CFCARDP		Set the card present flag
@cfinab	rts

* Read one sector. The LBA parameters are assumed to have been set previously.
* On input X points to the receiving (at least 512 bytes long) buffer.
CF1SRD	lda	CFSTATR
	bita	#CFDRQB		Check for DRQ bit in the CF status register
	beq	IOERR
	ldw	#CFSCSZ		Sector size is 512 bytes
	ldy	#CFDATAR	The data source address
	tfm	y,x+		Note: tfm is interruptible!
	rts
IOERR	ldb	#17
	jsr	ERRHDLR		No return
CFR1SRA	nop			For symbolic stack dump debugging purposes

* Write one sector. The LBA parameters are assumed to have been set previously.
* On input X points to the sending (at least 512 bytes long) buffer.
CF1SWR	lda	CFSTATR
	anda	#CFBSYB|CFDRQB
	cmpa	#CFDRQB		Check for BSY clear and DRQ set
	bne	CF1SWR
	ldw	#CFSCSZ		Sector size is 512 bytes
	ldy	#CFDATAR	The data destination address
	tfm	x+,y		Note: tfm is interruptible!
	rts

* 1KB Forth block support primitives. Not dictionary code. More like BIOS ops.

* One block IO preamble: select drive and LBA parameters. On input:
* ,s has the return address
* 2,s had the return address for the caller
* 4,s has the target block number
CF1BPRE	jsr	CFWAIT
	jsr	CFDRSEL		Set drive and head parameters
	ldd	4,s
	lsld			Convert block number to sector number
	stb	CFSNUMR		LBA 7-0
	sta	CFCLOWR		LBA 15-8
	tfr	cc,a
	anda	#CFLAG
	sta	CFCHIGR		LBA 23-16 (carry to cylinder high)
	lda	#2		2 IDE sectors per Forth block
	sta	CFSCNTR
	rts

* Read one block from CF device. On input:
* ,s has the return address
* 2,s has the target block number
* 4,s has the buffer base address
* Upon return, Y will be preserved.
CF1BKRD	tst	CFCARDP
	bne	@ctnued
	bra	IOERR		Card not present--CF subsystem not initialized
@ctnued tfr	y,v
	bsr	CF1BPRE
	ldb	#CFRSCTS	Issue a "Read sectors" CF command
	jsr	CFCMDIS
	ldx	4,s		Target buffer address to X
	bsr	CF1SRD		Read the even sector from CF device
	jsr	CFWAIT		Wait for next sector to become available
	bsr	CF1SRD		and read the odd sector
	tfr	v,y
	rts

* Write one block to CF device. Upon entry:
* ,s has the return address
* 2,s has the target block number
* 4,s has the buffer base address
* Upon return, Y will be preserved.
CF1BKWR tst	CFCARDP
	bne	@ctnued
	bra	IOERR		Card not present--CF subsystem not initialized
@ctnued	tfr	y,v
	bsr     CF1BPRE
	ldb	#CFWSCTS	Issue a "Write sectors" CF command
	jsr	CFCMDIS
	ldx	4,s		Source buffer address to X
	bsr	CF1SWR		Write the even sector to CF device
	jsr	CFWAIT		Wait for next sector to become available
	bsr	CF1SWR		and write the odd sector
	tfr	v,y
	rts


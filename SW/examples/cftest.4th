\ This code is not intended for public consumption.
\ This is essentially the prototype implementation
\ that has been recoded in assembly language for
\ inclusion in the EEPROM hex image.

$C000 CONSTANT cf.base \ CF task file base address
cf.base     CONSTANT cf.data \ R/W data register
cf.base 1+  CONSTANT cf.err  \ R error register
cf.base 1+  CONSTANT cf.feat \ W features register
cf.base 2 + CONSTANT cf.scnt \ R/W sector count register 
cf.base 3 + CONSTANT cf.snum \ R/W sector number register
cf.base 4 + CONSTANT cf.clow \ R/W cylinder low register
cf.base 5 + CONSTANT cf.chig \ R/W cylinder high register
cf.base 6 + CONSTANT cf.drhd \ R/W drive/head register
cf.base 7 + CONSTANT cf.stat \ R status register
cf.base 7 + CONSTANT cf.comd \ W command register

\ Implementation dependent: the VARIABLE overhead is
\ 9 + variable name length
: ALIGN16 ( namelen -- ) HERE + 9 + $F AND ?DUP IF
    NEGATE $10 + ALLOT
  THEN ;
9 ALIGN16 VARIABLE cf.buffer $3FE ALLOT   \ 1 blk, 1024 bytes
VARIABLE cf.command.mirror   \ For diagnostic purposes

: cf.wait ( -- ) BEGIN
    cf.stat C@ $80 AND 0= \ $80 is the BSY bit
  UNTIL ;

: cf.cmd.rdy ( -- ) cf.wait $1FFF 0 DO
    cf.stat C@ $40 AND IF UNLOOP EXIT THEN \ $40 is the RDY bit
  LOOP ." cf.cmd.rdy: timed out. Card not present?" QUIT ;

: cf.error.check ( -- ) cf.wait cf.stat C@ 1 AND UNLESS
    [CHAR] # EMIT EXIT  \ No errors dectected
  THEN
  [CHAR] ! EMIT cf.err C@ . \ Print error register contents
  ." [CMD was " cf.command.mirror C@ . [CHAR] ] EMIT ;

: cf.cmd.issue ( cmd -- ) cf.cmd.rdy  DUP cf.command.mirror C!
  cf.comd C!  cf.error.check ;

: cf.feature.set ( -- ) $EF cf.cmd.issue ;

: cf.drive.select ( -- ) cf.cmd.rdy
  $E0 cf.drhd C!  cf.cmd.rdy ;

\ Enable 8-bit data transfers
: cf.init ( -- ) cf.drive.select  1 cf.feat C!
  cf.feature.set ;

: cf.onesector.read ( addr -- ) cf.stat C@ 8 AND UNLESS
    ." cf.onesector.read: expected DRQ condition is not met"
    DROP QUIT
  THEN
  DUP $200 + SWAP DO
    cf.data C@ I C!
  LOOP ;

: cf.onesector.write ( addr -- )
  \ Make sure BSY is clear and DRQ is set
  BEGIN cf.stat C@ $88 AND 8 = UNTIL
    DUP $200 + SWAP DO
    I C@ cf.data C!
  LOOP ;

: cf.drive.identify ( -- ) cf.drive.select
  $EC cf.cmd.issue  \ Identify Drive
  HERE cf.onesector.read ;  \ Read one sector

: >< ( n -- m )    \ Exchange lower and upper bytes
  DUP $FF AND 8 SHIFT  SWAP  $FF00 AND -8 SHIFT OR ;

: cf.bytesinword.swapr ( addr 2bytescount -- ) 0 DO
    DUP DUP @ >< SWAP ! 2 +
  LOOP DROP ;

\ Remove leading spaces, some sort of -LEADING
: cf.ltrim ( addr bcount -- addr2 bcount2 ) 0 DO
    DUP I + C@ BL = UNLESS
      I' I - SWAP I + SWAP UNLOOP EXIT
    THEN
  LOOP 0 ;

: cf.this.analyze ( -- ) CR
  ." Model number      : " HERE $36 + $14 cf.bytesinword.swapr
    HERE $36 + $28 TYPE CR
  ." Serial number     : " HERE $14 + $14 cf.ltrim TYPE CR
  ." Firmware revision : " HERE $2E + 4 cf.bytesinword.swapr
    HERE $2E + 8 TYPE CR
  ." Sectors (LBA)     : " HERE $78 + @ >< HERE $7A + @ ><
    <# # # # # [CHAR] : HOLD # # # # #> TYPE CR ;

: cf.all ( -- ) cf.init cf.drive.identify cf.this.analyze ;

: cf.oneblock.preamble ( blknum -- )
  cf.wait cf.drive.select
  1 SHIFT  DUP $FF AND cf.snum C!  -8 SHIFT cf.clow C!
  0 cf.chig C!  2 cf.scnt C! ;

: cf.oneblock.read ( addr blknum -- )
  cf.oneblock.preamble
  $20 cf.cmd.issue \ Read sectors
  DUP cf.onesector.read cf.wait
  $200 + cf.onesector.read ;

: cf.oneblock.write ( addr blknum -- )
  cf.oneblock.preamble
  $30 cf.cmd.issue \ Write sectors
  DUP cf.onesector.write cf.wait
  $200 + cf.onesector.write ;

\ Write the first 1024 bytes of the EEPROM to block 0, blank
\ the buffer, read back sector 0 and dump the end result
: cf.test cf.all ( -- )
  $E000 cf.buffer $400 CMOVE
  cf.buffer 0 cf.oneblock.write
  cf.buffer $400 BLANKS
  cf.buffer 0 cf.oneblock.read
  cf.buffer $400 DUMP ;


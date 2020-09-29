: BENCHME ( xt n -- )
  DUP >R 0 DO
    DUP EXECUTE
    [CHAR] . EMIT
  LOOP
  DROP CR R> . ." Iterations." SPACE ;

HEX
( Adapted from "Hacker's Delight" Second Edition    )
( by Henry S. Warren Jr., Edt by Addison-Wesley     )
( Chapter 5 "Counting bits", page 82                )

: COUNTBITS ( uu -- #bits )
  DUP -1 SHIFT 5555 AND -
  DUP 3333 AND SWAP -2 SHIFT 3333 AND +
  DUP -4 SHIFT + 0F0F AND
  DUP -8 SHIFT +
  1F AND ;

DECIMAL
: BITSINCELL ( -- )
  8192 0 DO
    I COUNTBITS DROP
  LOOP ;

FIND BITSINCELL 100 BENCHME

\ @ 3 Mhz native: 8m20s for 100 rounds--5s per round
\ @ 4 Mhz native: 6m15s for 100 rounds--3.75s per round
\ @ 5 Mhz native: 5m for 100 rounds--3s per round


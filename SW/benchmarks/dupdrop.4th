: BENCHME ( xt n -- )
  DUP >R 0 DO
    DUP EXECUTE
    [CHAR] . EMIT
  LOOP
  DROP R> SPACE . ." Iterations." SPACE ;

\ -------------------------------------------------------------
\ DUP/DROP benchmark (custom).

DECIMAL
: DDBENCH 1 32767 0 DO DUP DROP LOOP DROP ;
\ FIND DDBENCH 100 BENCHME
' DDBENCH 100 BENCHME

\ 79-STANDARD @ 3 MHz native: 6m03s for 100 rounds--3.63s per round
\ 79-STANDARD @ 4 MHz native: 4m32s for 100 rounds--2.72s per round
\ 79-STANDARD @ 5 MHz native: 3m38s for 100 rounds--1.88s per round
\ ANS94 @ 4 MHz native: 4m34s for 100 rounds--2.74s per round

\ -------------------------------------------------------------


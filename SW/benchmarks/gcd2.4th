: BENCHME ( xt n -- )
  DUP >R 0 DO
    DUP EXECUTE
    [CHAR] . EMIT
  LOOP
  DROP R> SPACE . ." Iterations." SPACE ;

\ -------------------------------------------------------------
\ GCD2 algorithm (https://theultimatebenchmark.org).

DECIMAL
: GCD2 ( a b -- gcd )
  2DUP     D0= IF 2DROP 1 EXIT THEN
  DUP      0=  IF  DROP   EXIT THEN
  SWAP DUP 0=  IF  DROP   EXIT THEN
  BEGIN
    2DUP -
  WHILE
    2DUP < IF
      OVER -
    ELSE
      SWAP OVER - SWAP
    THEN
  REPEAT NIP ;

: GCD2-BENCH 100 0 DO
    100 0 DO
      J I GCD2 DROP
    LOOP
  LOOP ;

\ FIND GCD2-BENCH 10 BENCHME
' GCD2-BENCH 10 BENCHME
\ 79-STANDARD @ 3 MHz native: 4m59s for 10 rounds--29.9s per round
\ 79-STANDARD @ 4 MHz native: 3m44s for 10 rounds--22.4s per round
\ 79-STANDARD @ 5 MHz native: 2m59s for 10 rounds--17.9s per round
\ ANS94 @ 4 MHz native: 3m41s for 10 rounds--22.1s per round

\ -------------------------------------------------------------


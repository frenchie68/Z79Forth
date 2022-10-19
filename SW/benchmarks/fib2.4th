: BENCHME ( xt n -- )
  DUP >R 0 DO
    DUP EXECUTE
    [CHAR] . EMIT
  LOOP
  DROP R> SPACE . ." Iterations." SPACE ;

\ -------------------------------------------------------------
\ FIB2 benchmark (https://theultimatebenchmark.org)

DECIMAL
: FIB2
  0 1 ROT 0 DO
    OVER + SWAP
  LOOP DROP ;
: FIB2-BENCH 1000 0 DO
    I FIB2 DROP
  LOOP ;

\ FIND FIB2-BENCH 10 BENCHME
' FIB2-BENCH 10 BENCHME
\ 79-STANDARD @ 3 MHz native: 11m55s for 10 rounds--1m11.5s per round
\ 79-STANDARD @ 4 MHz native: 8m57s for 10 rounds--53.7s per round
\ 79-STANDARD @ 5 MHz native: 7m09s for 10 rounds--42.9s per round
\ ANS94 @ 4 MHz native: 9m57s for 10 rounds--59.7s per round

\ -------------------------------------------------------------


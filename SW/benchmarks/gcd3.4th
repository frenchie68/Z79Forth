: BENCHME ( xt n -- )
  DUP >R 0 DO
    DUP EXECUTE
    [CHAR] . EMIT
  LOOP
  DROP R> SPACE . ." Iterations." SPACE ;

\ -------------------------------------------------------------
\ GCD3 algorithm (provided by Paul E. Bennett).

DECIMAL
: GCD3 ( S: x y -- n) ( MCC=5 )
\ *G Find the Greatest Common Divisor "gcd" of two
\ ** integers "x and y", when at least one of them is
\ ** not zero. Return the largest positive integer "n"
\ ** that divides the two numbers without a remainder.
\ ** For example, the GCD of 8 and 12 is 4.
  2DUP <= IF
    SWAP
  THEN
  BEGIN
    TUCK MOD
    DUP 0=
  UNTIL DROP ;

: GCD3-BENCH 100 1 DO
    100 1 DO
      J I GCD3 DROP
    LOOP
  LOOP ;

\ FIND GCD3-BENCH 10 BENCHME
' GCD3-BENCH 10 BENCHME
\ 79-DTANDARD @ 3 MHz native: 1m1s for 10 rounds--6.1s per round
\ 79-STANDARD @ 4 MHz native: 46s for 10 rounds--4.6s per round
\ 79-STANDARD @ 5 MHz native: 37s for 10 rounds--3.7s per round
\ ANS94 @ 4 MHz native: 58s for 10 rounds--5.8s per round

\ -------------------------------------------------------------


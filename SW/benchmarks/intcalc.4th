: BENCHME ( xt n -- )
  DUP >R 0 DO
    DUP EXECUTE
    [CHAR] . EMIT
  LOOP
  DROP R> SPACE . ." Iterations." SPACE ;

\ -------------------------------------------------------------
\ IntCalc benchmark (https://theultimatebenchmark.org)

DECIMAL
32000 CONSTANT INTMAX

VARIABLE INTRESULT

: DOINT 
  1 DUP INTRESULT DUP >R !
  BEGIN 
    DUP INTMAX <
  WHILE 
    DUP NEGATE R@ +! 1+
    DUP R@ +! 1+
    R@ @ OVER * R@ ! 1+
    R@ @ OVER / R@ ! 1+
  REPEAT
  R> DROP DROP ;

\ FIND DOINT 100 BENCHME
' DOINT 100 BENCHME
\ 79-STANDARD @ 3 MHz native: 6m44s for 100 rounds--4.04s per round
\ 79-STANDARD @ 4 MHz native: 5m02s for 100 rounds--3.02s per round
\ 79-STANDARD @ 5 MHz native: 4m02s for 100 rounds--2.42s per round
\ ANS94 @ 4 MHz native: 5m30s for 100 rounds--3.30s per round

\ -------------------------------------------------------------


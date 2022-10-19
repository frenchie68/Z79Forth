: BENCHME ( xt n -- )
  DUP >R 0 DO
    DUP EXECUTE
    [CHAR] . EMIT
  LOOP
  DROP R> SPACE . ." Iterations." SPACE ;

\ -------------------------------------------------------------
\ GCD1 algorithm (https://theultimatebenchmark.org).

DECIMAL
: GCD1 OVER IF
    BEGIN
      DUP
    WHILE
      2DUP U> IF SWAP THEN OVER -
    REPEAT
    DROP
  ELSE
    DUP IF
      DROP
    ELSE
      2DROP 1
    THEN
  THEN ;

: GCD1-BENCH 100 0 DO
    100 0 DO
      J I GCD1 DROP
    LOOP
  LOOP ;

\ FIND GCD1-BENCH 10 BENCHME
' GCD1-BENCH 10 BENCHME
\ 79-STANDARD @ 3 MHz native: 3m55.3s for 10 rounds--25.53s per round
\ 79-STANDARD @ 4 MHz native: 2m56.5s for 10 rounds--17.65s per round
\ 79-STANDARD @ 5 MHz native: 2m21s for 10 rounds--14.1s per round
\ ANS94 @ 4 MHz native: 2m49s for 10 rounds--16.9s per round

\ -------------------------------------------------------------


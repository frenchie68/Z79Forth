: BENCHME ( xt n -- )
  DUP >R 0 DO
    DUP EXECUTE
    [CHAR] . EMIT
  LOOP
  DROP R> SPACE . ." Iterations." SPACE ;

\ -------------------------------------------------------------
\ [BENCHMARK] Glibreath's fixed algorithm:
\ Eratosthenes' sieve from ORNL/TM10656 (Martin Marietta).

DECIMAL
8190 CONSTANT SIZE
VARIABLE FLAGS SIZE 1+ ALLOT

: DO-PRIME
  FLAGS SIZE 1+ 1 FILL
  0 SIZE 0 DO
    FLAGS I + C@ IF
      I DUP + 3 + DUP I +
      BEGIN
        DUP SIZE <=
      WHILE
        0 OVER FLAGS + C! OVER +
      REPEAT
      DROP DROP 1+
    THEN
  LOOP DROP ;

FIND DO-PRIME 50 BENCHME
\ @ 3 MHz native: 3m45s--4.3s per round
\ @ 4 MHz native: 2m49s--3.4s per round
\ @ 5 MHz native: 2m15s--2.7s per round

\ -------------------------------------------------------------


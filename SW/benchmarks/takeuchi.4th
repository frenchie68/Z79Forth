: BENCHME ( xt n -- )
  DUP >R 0 DO
    DUP EXECUTE
    [CHAR] . EMIT
  LOOP
  DROP R> SPACE . ." Iterations." SPACE ;

\ -------------------------------------------------------------
\ Takeuchi algorithm (https://theultimatebenchmark.org).
\ Adapted from FORTH-83 (PICK). NIP is ANSI (and builtin).

DECIMAL
: 3DUP 3 PICK 3 PICK 3 PICK ;
: TAK ( x y z -- t )
  OVER 4 PICK < NEGATE IF
    NIP NIP EXIT
  THEN
  3DUP ROT  1- -ROT RECURSE >R
  3DUP SWAP 1- -ROT SWAP RECURSE >R
            1- -ROT RECURSE
  R> SWAP R>   -ROT RECURSE ;

: TAKBENCH ( -- )
  0 1000 0 DO
    DROP 18 12 6 TAK
  LOOP
  DROP ;

FIND TAKBENCH 200 BENCHME
\ @ 3 MHz native: 0m55s for 200 rounds--0.28s per round
\ @ 4 MHz native: 0m42s for 200 rounds--0.21s per round
\ @ 5 MHz native: 0m33s for 200 rounds--0.17s per round

\ -------------------------------------------------------------


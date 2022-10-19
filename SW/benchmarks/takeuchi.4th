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
: 3DUP 2 PICK 2 PICK 2 PICK ;
: TAK ( x y z -- t )
  OVER 3 PICK < NEGATE IF
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

\ FIND TAKBENCH 200 BENCHME ( 79-STANDARD )
' TAKBENCH 200 BENCHME ( ANS94 )
\ 79-STANDARD @ 3 MHz native: 0m55s for 200 rounds--0.28s per round
\ 79-STANDARD @ 4 MHz native: 0m42s for 200 rounds--0.21s per round
\ 79-STANDARD @ 5 MHz native: 0m33s for 200 rounds--0.17s per round
\ ANS94 @ 4 MHz native: 0m44s for 200 rounds--0.22s per round

\ -------------------------------------------------------------


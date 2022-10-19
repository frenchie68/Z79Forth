: BENCHME ( xt n -- )
  DUP >R 0 DO
    DUP EXECUTE
    [CHAR] . EMIT
  LOOP
  DROP R> SPACE . ." Iterations." SPACE ;

\ -------------------------------------------------------------
\ Nesting benchmark (https://theultimatebenchmark.org)

DECIMAL
: BOTTOM ;
: 1ST BOTTOM BOTTOM ;  : 2ND 1ST 1ST ;      : 3RD 2ND 2ND ;
: 4TH 3RD 3RD ;        : 5TH 4TH 4TH ;      : 6TH 5TH 5TH ;
: 7TH 6TH 6TH ;        : 8TH 7TH 7TH ;      : 9TH 8TH 8TH ;
: 10TH 9TH 9TH ;       : 11TH 10TH 10TH ;   : 12TH 11TH 11TH ;
: 13TH 12TH 12TH ;     : 14TH 13TH 13TH ;   : 15TH 14TH 14TH ;
: 16TH 15TH 15TH ;     : 17TH 16TH 16TH ;   : 18TH 17TH 17TH ;
: 19TH 18TH 18TH ;     : 20TH 19TH 19TH ;   : 21TH 20TH 20TH ;
: 22TH 21TH 21TH ;     : 23TH 22TH 22TH ;   : 24TH 23TH 23TH ;
: 25TH 24TH 24TH ;
: 32MILLION   25TH ;
:  1MILLION   20TH ;

\ FIND 1MILLION 10 BENCHME ( 79-STANDARD )
' 1MILLION 10 BENCHME ( ANS94 )
\ 79-STANDARD @ 3 MHz native: 0m49s for 10 rounds--4.9s per round
\ 79-STANDARD @ 4 MHz native: 0m37s for 10 rounds--3.7s per round
\ 79-STANDARD @ 5 MHz native: 0m29s for 10 rounds--2.9s per round
\ ANS94 @ 4 MHz native: 0m37s for 10 rounds--3.7s per round

\ FIND 32MILLION 10 BENCHME ( 79-STANDARD )
' 32MILLION 10 BENCHME ( ANS94 )
\ 79-STANDARD @ 3 MHz native: 26m06s for 10 rounds--2m37s per round
\ 79-STANDARD @ 4 MHz native: 19m35s for 10 rounds--1m57s per round
\ 79-STANDARD @ 5 MHz native: 15m40s for 10 rounds--1m34s per round
\ ANS94 @ 4 MHz native: 19m35s for 10 rounds--1m57s per round

\ -------------------------------------------------------------


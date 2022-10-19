: BENCHME ( xt n -- )
  DUP >R 0 DO
    DUP EXECUTE
    [CHAR] . EMIT
  LOOP
  DROP R> SPACE . ." Iterations." SPACE ;

\ -------------------------------------------------------------
\ MemMove benchmark (https://theultimatebenchmark.org)

DECIMAL
8192 CONSTANT BUFSIZE
VARIABLE BUF1 HERE BUFSIZE 1+ ALLOT BUF1 !
VARIABLE BUF2 HERE BUFSIZE 1+ ALLOT BUF2 !
: TEST-CMOVE 50 0 DO BUF1 @ BUF2 @ BUFSIZE CMOVE LOOP ;
: TEST-CMOVE> 50 0 DO BUF2 @ BUF1 @ BUFSIZE CMOVE> LOOP ;
: TEST-MOVE> 50 0 DO
    BUF1 @ BUF2 @ BUFSIZE MOVE
  LOOP ;
: TEST-<MOVE 50 0 DO
    BUF2 @ BUF1 @ BUFSIZE MOVE
  LOOP ;
: MOVE-BENCH TEST-CMOVE TEST-CMOVE> TEST-MOVE> TEST-<MOVE ;

\ FIND MOVE-BENCH 250 BENCHME
' MOVE-BENCH 250 BENCHME
\ 79-STANDARD @ 3 MHz native: 7m00s for 250 rounds--1.68s per round
\ 79-STANDARD @ 4 MHz native: 5m15s for 250 rounds--1.26s per round
\ 79-STANDARD @ 5 MHz native: 4m12s for 250 rounds--1s per round
\ ANS94 @ 4 MHz native: 5m15s for 250 rounds--1.26s per round

\ -------------------------------------------------------------


DECIMAL

: MYBOUNDS OVER + SWAP ;
: ALIGN ;              ( No such constraint on 16 bit targets )

1 CELLS CONSTANT CELL

VARIABLE SEED

: INITIATE-SEED ( -- )  4931 SEED ! ;
: RANDOM ( -- N )  SEED @ 613 * 5179 + DUP SEED ! ;

1500 CONSTANT ELEMENTS

ALIGN CREATE LIST ELEMENTS CELLS ALLOT

: INITIATE-LIST ( -- )
  LIST ELEMENTS CELLS + LIST DO
    RANDOM I !
  CELL +LOOP ;

: VERIFY-LIST ( -- )
  LIST ELEMENTS 1- CELLS MYBOUNDS DO
    I 2@ < IF
      ." BUBBLE-SORT: not sorted" ABORT
    THEN
  CELL +LOOP ;

: BUBBLE-WITH-FLAG ( -- )
  ." Bubbling..." CR
  1 ELEMENTS 1 DO
    [CHAR] . EMIT
    -1 LIST ELEMENTS I - CELLS MYBOUNDS DO
      I 2@ < IF
        I 2@ SWAP I 2! DROP 0
      THEN
    CELL +LOOP 
    IF LEAVE THEN
  LOOP SPACE DROP ;
  
: BUBBLE-SORT-WITH-FLAG ( -- )
  INITIATE-SEED
  INITIATE-LIST
  BUBBLE-WITH-FLAG
  VERIFY-LIST ;

\ 79-STANDARD @ 3 MHz native: 4m31s
\ 79-STANDARD @ 4 MHz native: 3m24s
\ 79-STANDARD @ 5 MHz native: 2m43s
\ ANS94 @ 4 MHz native: 3m23s

: DUMP-LIST ( -- )
  CR LIST ELEMENTS CELLS + LIST DO
    I @ 7 .R SPACE CELL
  +LOOP CR ;


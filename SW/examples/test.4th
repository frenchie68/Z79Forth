: TOTO
  IF
    ." true1 "
    IF ." true2 "
    ELSE ." false2 "
    THEN CR
  ELSE
    ." false1 "
    IF ." true3 "
    ELSE ." false3 "
    THEN CR
  THEN ;

: L+ ." +" 2 DO ." -" LOOP ." +" CR ;
: L! ." !" 2 DO ."  " LOOP ." !" CR ;
: REC CR
  OVER L+
  OVER SWAP 2 DO DUP L! LOOP DROP
  L+ ;
DECIMAL
20 4 REC

: JUSQUA BEGIN DUP . 1- DUP 0= UNTIL DROP ;
10 JUSQUA

: TANTQUE BEGIN DUP . 1- DUP WHILE [CHAR] % EMIT REPEAT DROP ;
10 TANTQUE

\ -------------------------------------------------------------
\ Recursion demo.

: FACTORIAL ( U -- U )
  DUP IF
    DUP 1- RECURSE *
  ELSE
    DROP 1
  THEN ;
: FACTORIAL-TABLE ( -- )
  9 1 DO
    CR I DUP 1 .R [CHAR] ! EMIT
    FACTORIAL 6 U.R
  LOOP SPACE ;

\ -------------------------------------------------------------
\ Pictured numbers demo.

DECIMAL
: 4BITS # # # # ;
: INSDOT [CHAR] . HOLD ;
: BIN16 BASE @ >R
  BIN DUP 0 <# 4BITS INSDOT 4BITS INSDOT
    4BITS INSDOT 4BITS #> TYPE
  R> BASE ! ;
: FOO16 [ BASE @ HEX ] CR
  3C00 BIN16 CR
   3FF BIN16 CR
  OR BIN16 DROP
  [ BASE ! ] ;
FOO16

\ -------------------------------------------------------------
\ D+ Testing (overkill, but efficient!).

DECIMAL
8 4 * 7 + CONSTANT B32LEN ( data + delimiters )
CREATE B32BUF B32LEN ALLOT
VARIABLE B32PTR
: 1-! 1 SWAP -! ;
: B32EMIT B32PTR @ C! B32PTR 1-! ;
: BIN32 B32BUF B32LEN BL FILL 2DUP
  B32BUF B32LEN + 1- B32PTR !
  SWAP 2 0 DO    ( LS cell then MS, converting toward low mem )
    16 0 DO
      DUP 1 AND IF [CHAR] 1 ELSE [CHAR] 0 THEN B32EMIT
      I 3 AND 3 = IF                ( output nibble separator )
        I 15 = J 0= AND IF
          [CHAR] : B32EMIT
        ELSE
          J 1 = I 15 = AND NOT IF [CHAR] . B32EMIT THEN
        THEN
      THEN
      -1 SHIFT
    LOOP DROP
  LOOP B32BUF B32LEN -TRAILING TYPE ;
: FOO32 CR
  2 SPACES 2SWAP BIN32 CR
  [CHAR] + EMIT SPACE 2SWAP BIN32 CR
  [CHAR] = EMIT SPACE D+ BIN32 2DROP ;
-1 -1 ( -1 in double format )
 1  0 ( 1 in double format )
FOO32 ( the sum must be zero )
-1 0  ( 65535 in double format )
 1 0  ( 1 in double format )
FOO32 ( 65536 in double format should be displayed )

\ -------------------------------------------------------------
\ IMMEDIATE/RESTRICT semantics verification

: TRUC DUP DUP DROP . ; IMMEDIATE
1 : TOTO TRUC ." toto" SPACE ;
TOTO

: FOO 5 . ;
FOO
RESTRICT
FOO            ( XXXX Incorrect STATE )

\ -------------------------------------------------------------
\ LOOP semantics verification

DECIMAL
: MACHIN 12 -7 DO
    I .
  LOOP CR ;		( 12 should _NOT_ appear)

\ -------------------------------------------------------------
\ Leave validation

DECIMAL
: FOO 10 0 DO
    I DUP . 7 = IF
      LEAVE
    THEN
  LOOP ;

\ -------------------------------------------------------------
\ From Alain Pinaud's "Programmer en Forth"

: 2DARR CREATE 1+ DUP ,
  SWAP 1+
  * 2* ALLOT
  DOES>
    ROT OVER @ * ROT + 2* + 2+ ;
8 4 2DARR DUMMY

\ -------------------------------------------------------------
\ From Leo Brodie's "Starting FORTH"

: CHARS CREATE
  DUP , ALLOT
  DOES>
    DUP 2+ SWAP @ ;
20 CHARS TOTO

\ -------------------------------------------------------------
\ More DOES> validation

: CST CREATE
  ,
  DOES>
    @ ;
HEX 20 CST BL
CR BL EMIT CHAR . EMIT
# The expected output is:
 .

\ -------------------------------------------------------------
\ U.R
HEX
VARIABLE OFFSET
: HDUMP DUP OFFSET !
  DO
    I OFFSET @ - F AND 0= IF
      CR I 4 U.R SPACE
    THEN
    I C@ 2 U.R SPACE
  LOOP
  CR ;
HERE LAST HDUMP

\ -------------------------------------------------------------
\ Pictured numbers output validation.
HEX
-1 DUP <# # # # # # # # # #> TYPE
0 8000 <# # # # # # # # # #> TYPE
0 7FFF <# # # # # # # # # #> TYPE
3C00 0 <# # # # # # # # # #> TYPE

DECIMAL
-1 DUP <# # # # 46 HOLD # # # 46 HOLD # # # 46 HOLD # #>
TYPE
# Expected output is 
TYPE 4.294.967.295 OK

\ -------------------------------------------------------------
\ BEGIN/AGAIN testing
: TRUC BEGIN DUP 7 > IF DROP EXIT ELSE DUP . 1+ THEN AGAIN ;
0 TRUC .S
# Expedted output is
0 TRUC .S 0 1 2 3 4 5 6 7  OK

\ -------------------------------------------------------------
\ -trailing test
DECIMAL
32 CONSTANT BUFLEN
VARIABLE BUFF BUFLEN ALLOT
BUFF BUFLEN 1+ 32 FILL
BUFF BUFLEN -TRAILING TYPE		( emtpy string is printed )
BUFF BUFLEN EXPECT			( Abracadabra )
BUFF BUFLEN 1+ -TRAILING TYPE CHAR . EMIT

\ -------------------------------------------------------------
\ "Hacker's delight", second edition, chapter 2.1, page 11
\ wizzardry:
VARIABLE X
BIN
1100100 X !
: BIN8 0 CR <# [ DECIMAL ] 8 0 DO # LOOP #> TYPE SPACE [ BIN ] ;
\ Turn off the rightmost set bit in a word.
X @ DUP BIN8 DUP 1- DUP BIN8 AND BIN8

\ -------------------------------------------------------------
\ Print the last defined word's name (implemention dependent).

DECIMAL
: WHERE LAST DUP 1+ SWAP C@ 31 AND TYPE ;

\ -------------------------------------------------------------
\ WORD testing.

: .COUNTEDSTRING COUNT TYPE [CHAR] . EMIT CR ;
CHAR " WORD "Schnaps, das war sein letztes Wort"
." #1 " .COUNTEDSTRING

CHAR " WORD dann trugen ihn die Englein fort"
." #2 " .COUNTEDSTRING

\ This cannot be tested from blocks.
CHAR " WORD Schnaps das war sein letztes Wort
." #3 " .COUNTEDSTRING

\ This cannot be tested from blocks.
1 WORD
." #4 " .COUNTEDSTRING		( only . should be printed )

\ -------------------------------------------------------------
\ KEY?, MS

\ Exercise the KEY? predicate.
\ A non-blocking poll of the ACIA input
: POLLKEY ( -- )
  BEGIN
    KEY? IF
      SPACE KEY EMIT SPACE EXIT
    THEN
    [CHAR] . EMIT
    200 MS ( Wait for a fifth of a second )
  AGAIN ;
POLLKEY

\ -------------------------------------------------------------
\ MS calibration. Expected execution time is 1m40s.

DECIMAL
: DELAY 20 0 DO 5000 MS [CHAR] . EMIT LOOP SPACE ;

\ -------------------------------------------------------------
\ Verifying CHAR, [CHAR]

CHAR Arghh EMIT SPACE	( A is output )
: TOTO CHAR EMIT SPACE ;
TOTO Arghh 		( A is output )
: TRUC [CHAR] Arghh EMIT SPACE ;
TRUC			( A is output )
[CHAR]			( Error 6: incorrect STATE )

\ -------------------------------------------------------------
\ Exercising ?DUP

: QDUP-TRY ( n -- )
  BEGIN
    ?DUP
  WHILE
    DUP .
    1-
  REPEAT ;
8 QDUP-TRY		( Prints 8 down to 1 sequence )
0 QDUP-TRY
.S

\ -------------------------------------------------------------
\ Using [COMPILE] (ANSI calls this POSTPONE) to define ENDIF
\ as a replacement for THEN.

: ENDIF [COMPILE] THEN ; IMMEDIATE RESTRICT
: FOO CR IF ." NZ" ELSE ." Z" ENDIF ;
0 FOO ( Z is printed )
1 FOO ( NZ is printed )

\ -------------------------------------------------------------
\ Xmas ASCII art (Michel Jean).

: SAPIN ( -- ) CR 12 0 DO
  12 I - SPACES I 1+ DUP 0 DO [CHAR] / EMIT LOOP
  0 DO [CHAR] \ EMIT LOOP CR
  LOOP [CHAR] | DUP 2 0 DO
  12 0 DO [CHAR] ~ EMIT LOOP
  I 1 <> IF 2 0 DO EMIT LOOP THEN
  LOOP CR 12 SPACES 2 0 DO [CHAR] | EMIT
  LOOP 12 SPACES ;

\ -------------------------------------------------------------
\ EXIT optimization tests:
: bar EXIT ;     \ Degenerate case, RTS is emitted twice.
                 \ As is the case for any trailing EXIT during
                 \ a word definition.

: bar IF [CHAR] T EMIT EXIT ELSE [CHAR] F EMIT THEN [CHAR] . EMIT ;
\ Some dead code is emitted by ELSE at the end of the IF branch.

: bar if [CHAR] T EMIT EXIT ELSE [CHAR] F EMIT THEN ;
\ Some dead code is emitted by ELSE at the end of the IF branch.

: bar IF [CHAR] T EMIT EXIT THEN 
  [CHAR] F EMIT ;
\ Optimal code generation.

: bar 10 0 DO I DUP . 7 = IF EXIT THEN LOOP ;
\ Optimization does not apply.

: bar 10 0 DO I DUP . 7 = IF [CHAR] * EMIT EXIT THEN LOOP ;
\ Optimal code generation.


\ checks that postpone works correctly with words with special
\ compilation semantics

\ by M. Anton Ertl 1996

\ This file is based on John Hayes' core.fr (coretest.fs), which has
\ the following copyright notice:

\ (C) 1995 JOHNS HOPKINS UNIVERSITY / APPLIED PHYSICS LABORATORY
\ MAY BE DISTRIBUTED FREELY AS LONG AS THIS COPYRIGHT NOTICE REMAINS.

\ my contributions to this file are in the public domain

\ These tests are especially useful for showing that state-smart
\ implementations of words with special compilation semantics,
\ combined with a straight-forward implementation of POSTPONE (and
\ [COMPILE]) do not conform to the ANS Forth standard. The essential
\ sentences in the standad are:

\ 6.1.2033 POSTPONE CORE
\ ...
\ Compilation: ( <spaces>name -- ) 

\ Skip leading space delimiters. Parse name delimited by a space. Find
\ name. Append the compilation semantics of name to the current
\ definition.

\ 6.2.2530 [COMPILE] bracket-compile CORE EXT 
\ ...
\ Compilation: ( <spaces>name -- ) 

\ Skip leading space delimiters. Parse name delimited by a space. Find
\ name. If name has other than default compilation semantics, append
\ them to the current definition;...


\ Note that the compilation semantics are appended, not some
\ state-dependent semantics.

\ You have to include the T{ variants of tester.fr and core.fr (e.g.,
\ from http://soton.mpeforth.com/flag/anstests/anstests0.8.zip).

\ first I test against a non-ANS solution suggested by Bernd Paysan

: STATE@-NOW ( -- F )
    STATE @ ; IMMEDIATE

: STATE@ ( -- F )
    POSTPONE STATE@-NOW ;

T{ STATE@ 0<> -> TRUE }T

\ here I test POSTPONE with all core words with special compilation
\ semantics.

TESTING POSTPONE (

: POSTPONE-(
    POSTPONE ( ;

T{ : PP1 [ POSTPONE-( DOES NOTHING ) ] ; -> }T
T{ HERE PP1 -> HERE }T

TESTING POSTPONE +LOOP

: POSTPONE-+LOOP
    POSTPONE +LOOP ;

T{ : PGD2 DO I -1 [ POSTPONE-+LOOP ] ; -> }T
T{ 1 4 PGD2 -> 4 3 2 1 }T
T{ -1 2 PGD2 -> 2 1 0 -1 }T
T{ MID-UINT MID-UINT+1 PGD2 -> MID-UINT+1 MID-UINT }T

T{ : PGD4 DO 1 0 DO J LOOP -1 [ POSTPONE-+LOOP ] ; -> }T
T{ 1 4 PGD4 -> 4 3 2 1 }T
T{ -1 2 PGD4 -> 2 1 0 -1 }T
T{ MID-UINT MID-UINT+1 PGD4 -> MID-UINT+1 MID-UINT }T

TESTING POSTPONE ."

: POSTPONE-."
    POSTPONE ." ;

: PDQ2 [ POSTPONE-." YOU SHOULD SEE THIS LATER. " ] CR ;
: PDQ1 [ POSTPONE-." YOU SHOULD SEE THIS FIRST. " ] CR ;
T{ PDQ1 PDQ2 -> }T

TESTING POSTPONE ;
: POSTPONE-;
    POSTPONE ; ;

T{ : PSC [ POSTPONE-; -> }T
T{ PSC -> }T    

TESTING POSTPONE ABORT"

: POSTPONE-ABORT"
    POSTPONE ABORT" ;

T{ : PAQ1 [ POSTPONE-ABORT" THIS SHOULD NOT ABORT" ] ; -> }T

TESTING POSTPONE BEGIN
: POSTPONE-BEGIN
    POSTPONE BEGIN ;

T{ : PB3 [ POSTPONE-BEGIN ] DUP 5 < WHILE DUP 1+ REPEAT ; -> }T
T{ 0 PB3 -> 0 1 2 3 4 5 }T
T{ 4 PB3 -> 4 5 }T
T{ 5 PB3 -> 5 }T
T{ 6 PB3 -> 6 }T

T{ : PB4 [ POSTPONE-BEGIN ] DUP 1+ DUP 5 > UNTIL ; -> }T
T{ 3 PB4 -> 3 4 5 6 }T
T{ 5 PB4 -> 5 6 }T
T{ 6 PB4 -> 6 7 }T

T{ : PB5 [ POSTPONE-BEGIN ] DUP 2 > WHILE DUP 5 < WHILE DUP 1+ REPEAT 123 ELSE 345 THEN ; -> }T
T{ 1 PB5 -> 1 345 }T
T{ 2 PB5 -> 2 345 }T
T{ 3 PB5 -> 3 4 5 123 }T
T{ 4 PB5 -> 4 5 123 }T
T{ 5 PB5 -> 5 123 }T

TESTING POSTPONE DO
: POSTPONE-DO
    POSTPONE DO ;

T{ : PDO1 [ POSTPONE-DO ] I LOOP ; -> }T
T{ 4 1 PDO1 -> 1 2 3 }T
T{ 2 -1 PDO1 -> -1 0 1 }T
T{ MID-UINT+1 MID-UINT PDO1 -> MID-UINT }T

T{ : PDO2 [ POSTPONE-DO ] I -1 +LOOP ; -> }T
T{ 1 4 PDO2 -> 4 3 2 1 }T
T{ -1 2 PDO2 -> 2 1 0 -1 }T
T{ MID-UINT MID-UINT+1 PDO2 -> MID-UINT+1 MID-UINT }T

T{ : PDO3 [ POSTPONE-DO ] 1 0 [ POSTPONE-DO ] J LOOP LOOP ; -> }T
T{ 4 1 PDO3 -> 1 2 3 }T
T{ 2 -1 PDO3 -> -1 0 1 }T
T{ MID-UINT+1 MID-UINT PDO3 -> MID-UINT }T

T{ : PDO4 [ POSTPONE-DO ] 1 0 [ POSTPONE-DO ] J LOOP -1 +LOOP ; -> }T
T{ 1 4 PDO4 -> 4 3 2 1 }T
T{ -1 2 PDO4 -> 2 1 0 -1 }T
T{ MID-UINT MID-UINT+1 PDO4 -> MID-UINT+1 MID-UINT }T

T{ : PDO5 123 SWAP 0 [ POSTPONE-DO ] I 4 > IF DROP 234 LEAVE THEN LOOP ; -> }T
T{ 1 PDO5 -> 123 }T
T{ 5 PDO5 -> 123 }T
T{ 6 PDO5 -> 234 }T

T{ : PDO6  ( PAT: {0 0},{0 0}{1 0}{1 1},{0 0}{1 0}{1 1}{2 0}{2 1}{2 2} )
   0 SWAP 0 [ POSTPONE-DO ]
      I 1+ 0 [ POSTPONE-DO ] I J + 3 = IF I UNLOOP I UNLOOP EXIT THEN 1+ LOOP
    LOOP ; -> }T
T{ 1 PDO6 -> 1 }T
T{ 2 PDO6 -> 3 }T
T{ 3 PDO6 -> 4 1 2 }T

TESTING POSTPONE DOES>
: POSTPONE-DOES>
    POSTPONE DOES> ;

T{ : PDOES1 [ POSTPONE-DOES> ] @ 1 + ; -> }T
T{ : PDOES2 [ POSTPONE-DOES> ] @ 2 + ; -> }T
T{ CREATE PCR1 -> }T
T{ PCR1 -> HERE }T
T{ ' PCR1 >BODY -> HERE }T
T{ 1 , -> }T
T{ PCR1 @ -> 1 }T
T{ PDOES1 -> }T
T{ PCR1 -> 2 }T
\ Multiple DOES> clauses are not supported under Z79Forth.
\ T{ PDOES2 -> }T
\ T{ PCR1 -> 3 }T

\ T{ : PWEIRD: CREATE [ POSTPONE-DOES> ] 1 + [ POSTPONE-DOES> ] 2 + ; -> }T
\ T{ PWEIRD: PW1 -> }T
\ T{ ' PW1 >BODY -> HERE }T
\ T{ PW1 -> HERE 1 + }T
\ T{ PW1 -> HERE 2 + }T

TESTING POSTPONE ELSE
: POSTPONE-ELSE
    POSTPONE ELSE ;

T{ : PELSE1 IF 123 [ POSTPONE-ELSE ] 234 THEN ; -> }T
T{ 0 PELSE1 -> 234 }T
T{ 1 PELSE1 -> 123 }T

T{ : PELSE2 BEGIN DUP 2 > WHILE DUP 5 < WHILE DUP 1+ REPEAT 123 [ POSTPONE-ELSE ] 345 THEN ; -> }T
T{ 1 PELSE2 -> 1 345 }T
T{ 2 PELSE2 -> 2 345 }T
T{ 3 PELSE2 -> 3 4 5 123 }T
T{ 4 PELSE2 -> 4 5 123 }T
T{ 5 PELSE2 -> 5 123 }T

TESTING POSTPONE IF
: POSTPONE-IF
    POSTPONE IF ;

T{ : PIF1 [ POSTPONE-IF ] 123 THEN ; -> }T
T{ : PIF2 [ POSTPONE-IF ] 123 ELSE 234 THEN ; -> }T
T{ 0 PIF1 -> }T
T{ 1 PIF1 -> 123 }T
T{ -1 PIF1 -> 123 }T
T{ 0 PIF2 -> 234 }T
T{ 1 PIF2 -> 123 }T
T{ -1 PIF1 -> 123 }T

T{ : PIF6 ( N -- 0,1,..N ) DUP [ POSTPONE-IF ] DUP >R 1- RECURSE R> THEN ; -> }T
T{ 0 PIF6 -> 0 }T
T{ 1 PIF6 -> 0 1 }T
T{ 2 PIF6 -> 0 1 2 }T
T{ 3 PIF6 -> 0 1 2 3 }T
T{ 4 PIF6 -> 0 1 2 3 4 }T

TESTING POSTPONE LITERAL
: POSTPONE-LITERAL
    POSTPONE LITERAL ;

T{ : PLIT [ 42 POSTPONE-LITERAL ] ; -> }T
T{ PLIT -> 42 }T

TESTING POSTPONE LOOP
: POSTPONE-LOOP
    POSTPONE LOOP ;

T{ : PLOOP1 DO I [ POSTPONE-LOOP ] ; -> }T
T{ 4 1 PLOOP1 -> 1 2 3 }T
T{ 2 -1 PLOOP1 -> -1 0 1 }T
T{ MID-UINT+1 MID-UINT PLOOP1 -> MID-UINT }T

T{ : PLOOP3 DO 1 0 DO J [ POSTPONE-LOOP ] [ POSTPONE-LOOP ] ; -> }T
T{ 4 1 PLOOP3 -> 1 2 3 }T
T{ 2 -1 PLOOP3 -> -1 0 1 }T
T{ MID-UINT+1 MID-UINT PLOOP3 -> MID-UINT }T

T{ : PLOOP4 DO 1 0 DO J [ POSTPONE-LOOP ] -1 +LOOP ; -> }T
T{ 1 4 PLOOP4 -> 4 3 2 1 }T
T{ -1 2 PLOOP4 -> 2 1 0 -1 }T
T{ MID-UINT MID-UINT+1 PLOOP4 -> MID-UINT+1 MID-UINT }T

T{ : PLOOP5 123 SWAP 0 DO I 4 > IF DROP 234 LEAVE THEN [ POSTPONE-LOOP ] ; -> }T
T{ 1 PLOOP5 -> 123 }T
T{ 5 PLOOP5 -> 123 }T
T{ 6 PLOOP5 -> 234 }T

T{ : PLOOP6  ( PAT: {0 0},{0 0}{1 0}{1 1},{0 0}{1 0}{1 1}{2 0}{2 1}{2 2} )
   0 SWAP 0 DO
      I 1+ 0 DO I J + 3 = IF I UNLOOP I UNLOOP EXIT THEN 1+ [ POSTPONE-LOOP ]
    [ POSTPONE-LOOP ] ; -> }T
T{ 1 PLOOP6 -> 1 }T
T{ 2 PLOOP6 -> 3 }T
T{ 3 PLOOP6 -> 4 1 2 }T

TESTING POSTPONE POSTPONE
: POSTPONE-POSTPONE
    POSTPONE POSTPONE ;

T{ : PPP1 123 ; -> }T
T{ : PPP4 [ POSTPONE-POSTPONE PPP1 ] ; IMMEDIATE -> }T
T{ : PPP5 PPP4 ; -> }T
T{ PPP5 -> 123 }T
T{ : PPP6 345 ; IMMEDIATE -> }T
T{ : PPP7 [ POSTPONE-POSTPONE PPP6 ] ; -> }T
T{ PPP7 -> 345 }T

TESTING POSTPONE RECURSE
: POSTPONE-RECURSE
    POSTPONE RECURSE ;

T{ : GREC ( N -- 0,1,..N ) DUP IF DUP >R 1- [ POSTPONE-RECURSE ] R> THEN ; -> }T
T{ 0 GREC -> 0 }T
T{ 1 GREC -> 0 1 }T
T{ 2 GREC -> 0 1 2 }T
T{ 3 GREC -> 0 1 2 3 }T
T{ 4 GREC -> 0 1 2 3 4 }T

TESTING POSTPONE REPEAT
: POSTPONE-REPEAT
    POSTPONE REPEAT ;

T{ : PREP3 BEGIN DUP 5 < WHILE DUP 1+ [ POSTPONE-REPEAT ] ; -> }T
T{ 0 PREP3 -> 0 1 2 3 4 5 }T
T{ 4 PREP3 -> 4 5 }T
T{ 5 PREP3 -> 5 }T
T{ 6 PREP3 -> 6 }T

T{ : PREP5 BEGIN DUP 2 > WHILE DUP 5 < WHILE DUP 1+ [ POSTPONE-REPEAT ] 123 ELSE 345 THEN ; -> }T
T{ 1 PREP5 -> 1 345 }T
T{ 2 PREP5 -> 2 345 }T
T{ 3 PREP5 -> 3 4 5 123 }T
T{ 4 PREP5 -> 4 5 123 }T
T{ 5 PREP5 -> 5 123 }T

TESTING POSTPONE S"
: POSTPONE-S"
    POSTPONE S" ;

T{ : PSQ4 [ POSTPONE-S" XY" ] ; -> }T
T{ PSQ4 SWAP DROP -> 2 }T
T{ PSQ4 DROP DUP C@ SWAP CHAR+ C@ -> CHAR X CHAR Y }T

TESTING POSTPONE THEN
: POSTPONE-THEN
    POSTPONE THEN ;

T{ : PTH1 IF 123 [ POSTPONE-THEN ] ; -> }T
T{ : PTH2 IF 123 ELSE 234 [ POSTPONE-THEN ] ; -> }T
T{ 0 PTH1 -> }T
T{ 1 PTH1 -> 123 }T
T{ -1 PTH1 -> 123 }T
T{ 0 PTH2 -> 234 }T
T{ 1 PTH2 -> 123 }T
T{ -1 PTH1 -> 123 }T

T{ : PTH5 BEGIN DUP 2 > WHILE DUP 5 < WHILE DUP 1+ REPEAT 123 ELSE 345 [ POSTPONE-THEN ] ; -> }T
T{ 1 PTH5 -> 1 345 }T
T{ 2 PTH5 -> 2 345 }T
T{ 3 PTH5 -> 3 4 5 123 }T
T{ 4 PTH5 -> 4 5 123 }T
T{ 5 PTH5 -> 5 123 }T

T{ : PTH6 ( N -- 0,1,..N ) DUP IF DUP >R 1- RECURSE R> [ POSTPONE-THEN ] ; -> }T
T{ 0 PTH6 -> 0 }T
T{ 1 PTH6 -> 0 1 }T
T{ 2 PTH6 -> 0 1 2 }T
T{ 3 PTH6 -> 0 1 2 3 }T
T{ 4 PTH6 -> 0 1 2 3 4 }T

TESTING POSTPONE UNTIL
: POSTPONE-UNTIL
    POSTPONE UNTIL ;

T{ : PUNT4 BEGIN DUP 1+ DUP 5 > [ POSTPONE-UNTIL ] ; -> }T
T{ 3 PUNT4 -> 3 4 5 6 }T
T{ 5 PUNT4 -> 5 6 }T
T{ 6 PUNT4 -> 6 7 }T

TESTING POSTPONE WHILE
: POSTPONE-WHILE
    POSTPONE WHILE ;

T{ : PWH3 BEGIN DUP 5 < [ POSTPONE-WHILE ] DUP 1+ REPEAT ; -> }T
T{ 0 PWH3 -> 0 1 2 3 4 5 }T
T{ 4 PWH3 -> 4 5 }T
T{ 5 PWH3 -> 5 }T
T{ 6 PWH3 -> 6 }T

T{ : PWH5 BEGIN DUP 2 > [ POSTPONE-WHILE ] DUP 5 < [ POSTPONE-WHILE ] DUP 1+ REPEAT 123 ELSE 345 THEN ; -> }T
T{ 1 PWH5 -> 1 345 }T
T{ 2 PWH5 -> 2 345 }T
T{ 3 PWH5 -> 3 4 5 123 }T
T{ 4 PWH5 -> 4 5 123 }T
T{ 5 PWH5 -> 5 123 }T


TESTING POSTPONE [
: POSTPONE-[
    POSTPONE [ ;

T{ HERE POSTPONE-[ -> HERE }T

TESTING POSTPONE [']
: POSTPONE-[']
    POSTPONE ['] ;

T{ : PTICK1 123 ; -> }T
T{ : PTICK2 [ POSTPONE-['] PTICK1 ] ; IMMEDIATE -> }T
T{ PTICK2 EXECUTE -> 123 }T

TESTING POSTPONE [CHAR]
: POSTPONE-[CHAR]
    POSTPONE [CHAR] ;

T{ : PCHAR1 [ POSTPONE-[CHAR] X ] ; -> }T
T{ : PCHAR2 [ POSTPONE-[CHAR] HELLO ] ; -> }T
T{ PCHAR1 -> CHAR X }T
T{ PCHAR2 -> CHAR H }T


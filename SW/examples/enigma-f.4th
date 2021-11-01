\ Enigma Prototype. Bill Ragsdale. May 22nd, 2021.
\
\ 20-03-22-A First cut, works
\ 21-03-27-B Revised rotors, added turnovers and init. settings
\ 21-03-27-C Revised rotors to differential.  OK.
\ 21-04-01-D Added frequency table
\ 21-05-11-E Redesign 0 based array and rotor access.  OK.
\ 21-05-17-F Message processing. Working.
\ 21-06-09   Z79Forth adaptations, based original ANSI code

\ anew project  decimal  reset-stacks cls
DECIMAL PAGE NCLR RCLR

\ Z79Forth glue code.
: .( [CHAR] ) WORD COUNT TYPE ; IMMEDIATE
: ," [CHAR] " WORD C@ 1+ ALLOT ;
: Z," [CHAR] " WORD DUP C@ DUP >R \ addr bcount R: bcount
  SWAP DUP 1+ SWAP ROT CMOVE
  R> ALLOT  0 C, ;
: BODY> ( apf -- acf ) 9 - ; \ Applies only to CREATEd words
: .NAME ( acf -- ) .' SPACE ;
: ABORT" POSTPONE IF 
  POSTPONE ." 
  POSTPONE ABORT 
  POSTPONE THEN ; IMMEDIATE RESTRICT
: ERASE 0 FILL ;
: CELLS+ ( addr1 ncells -- addr2 ) CELLS + ;

26 CONSTANT #letters

CREATE Keyboard Z," ABCDEFGHIJKLMNOPQRSTUVWXYZ"
\ This is a zero based string without a count.
\   0   1   2   3   4   5   6   7   8   9  10  11  12
\   A   B   C   D   E   F   G   H   I   J   K   L   M
\   13 14  15  16  17  18  19  20  21  22  23  24  25
\   N   O   P   Q   R   S   T   U   V   W   X   Y   Z

CR CR .( Dump Keyboard; runs 41..5A 00 )
CR   Keyboard  #letters 1+  DUMP

: Xload ( n0..n25 addr -- )
\ load #letters values into an array, descending, offset 0..25.
  0  #letters 1-  DO SWAP OVER I + C! -1  +LOOP DROP ;

: SignExtend ( byte -- cell ) \ sign extend 8 bits to a cell
  DUP 128 AND IF -256 OR THEN ;

: Sc@ ( addr -- n1 ) \ Fetch a byte and sign extend
  C@  SignExtend ;

: bounded   #letters MOD ;  \ keep in 0..25 range

CR CR .( The Stecker is in alphabet order, no offsets.)

CREATE SteckerFwd #letters 1+ ALLOT
\ The letter relative offset forward or backward.
\    A   B   C   D   E   F   G   H   I   J   K   L   M
     0   0   0   0   0   0   0   0   0   0   0   0   0
\    N   O   P   Q   R   S   T   U   V   W   X   Y   Z
     0   0   0   0   0   0   0   0   0   0   0   0   0
SteckerFwd Xload

CR CR SteckerFwd #letters 1+ DUMP

: RotorInverse ( add1 addr2  -- )
\ copy rotor1 output offsets to rotor1 inverting the offsets.
  #letters 0 DO   OVER I +   Sc@ DUP NEGATE SWAP
                3 PICK I +  + C!   LOOP 2DROP ;

CREATE SteckerRev #letters 1+ ALLOT
SteckerFwd SteckerRev RotorInverse

CR CR SteckerRev #letters 1+ DUMP

CREATE RotorA-Fwd  #letters 1+ ALLOT \ Rotor One Forward links
\ value in selects value out adjusted for rotor position
\  0   1   2   3   4   5   6   7   8   9  10  11  12  letter in
   1   1   1   1   1  -5   1   1   1   1   1  -5   1
\ 13  14  15  16  17  18  19  20  21  22  23  24  25  letter in
   1   1   1   1  -5   1   1   1   1   1  -5   1  -1
RotorA-Fwd Xload

CR CR .( Look at RotorOneFwd and RotorOneRev )
CR RotorA-Fwd #letters 1+ DUMP

CREATE RotorA-Rev #letters 1+ ALLOT
       RotorA-Fwd RotorA-Rev RotorInverse

CR CR RotorA-Rev #letters 1+ DUMP

CREATE  Reflector #letters 1+ ALLOT \ only a single array
\   0   1   2   3   4   5   6   7   8   9  10  11  12
   13  13  13  13  13  13  13  13  13  13  13  13  13
\  13  14  15  16  17  18  19  20  21  22  23  24  25
  -13 -13 -13 -13 -13 -13 -13 -13 -13 -13 -13 -13 -13
Reflector Xload

CR CR .( Look at the Reflector. Runs  0D ... F3 )
CR Reflector #letters 1+ DUMP

\ Keyboard input
\   0   1   2   3   4   5   6   7   8   9  10  11  12
\   A   B   C   D   E   F   G   H   I   J   K   L   M
\  13  14  15  16  17  18  19  20  21  22  23  24  25
\   N   O   P   Q   R   S   T   U   V   W   X   Y   Z

CR CR .( Create the three slots to hold rotors)
: Define-Slot ( RotorxFwd RotorxRev Its_Position Its_Turnover )
  CREATE 4 CELLS ALLOT
  ( offset -- field within a Slot)
  DOES> + ; \ Yield the field address within this array's data

Define-Slot  SlotI
Define-Slot  SlotII
Define-Slot  SlotIII
Define-Slot  ReflectorI

( To access Slot parameters )
     0  CONSTANT  RotorForward
1 CELLS CONSTANT  RotorReverse
2 CELLS CONSTANT  RotorPosition
3 CELLS CONSTANT  RotorTurnover

CR CR .( Assign rotors A, B, C to the slots I, II and III)
: Start ( -- )  \ reset all rotors to their zero position
     RotorA-Fwd  RotorForward  SlotI    !
     RotorA-Rev  RotorReverse  SlotI    !
              0  RotorPosition SlotI    !
              0  RotorTurnover SlotI    !
     RotorA-Fwd  RotorForward  SlotII   !
     RotorA-Rev  RotorReverse  SlotII   !
              0  RotorPosition SlotII   !
              0  RotorTurnover SlotII   !
     RotorA-Fwd  RotorForward  SlotIII  !
     RotorA-Rev  RotorReverse  SlotIII  !
              0  RotorPosition SlotIII  !
              0  RotorTurnover SlotIII  !
      Reflector  RotorForward  ReflectorI  !
      Reflector  RotorReverse  ReflectorI  !
              0  RotorPosition ReflectorI  !
              0  RotorTurnover ReflectorI  !   ;
Start  .( Set default rotor and settings.)

CR CR .( Report the contents of Slots I, II and III )
: .slot ( address -- )  \ report contents of one slot
   CR  DUP RotorForward + @ BODY> .NAME
       DUP RotorReverse + @ BODY> .NAME
       RotorPosition + 2@ . .  ;

: .slots ( -- ) \ report contents of 3 slots
  0 SlotI .slot  0 SlotII .slot  0 SlotIII .slot
  0 ReflectorI .slot ;

: >step ( n -- n+1_mod_26)  \ increment within 0..25
   1+ #letters MOD ;

: EntryComplete ( -- ) \ increment SlotIII with turnovers
  RotorPosition SlotIII @ >step DUP RotorPosition SlotIII !
  RotorTurnover SlotIII @ =
  IF  RotorPosition SlotII @ >step DUP RotorPosition SlotII !
      RotorTurnover SlotII @ =
      IF  RotorPosition SlotI @ >step RotorPosition SlotI  !
  THEN THEN ;

: r?  \ Show the current rotor positions of Slots I, II and III
  CR RotorPosition SlotI   ? RotorPosition SlotII ?
     RotorPosition SlotIII ? ;

: CycleTest ( -- ) \ run through a cycle
    r? 8 0 DO EntryComplete r? LOOP ;

CR CR .( Checks turnovers SlotIII to SlotII)
Start 20 RotorPosition SlotIII ! 23 RotorTurnover SlotIII !
      25 RotorPosition SlotII  !  4 RotorTurnover SlotII  !
       3 RotorPosition SlotI   !  6 RotorTurnover SlotI   !
       CycleTest

CR CR  .( Checks SlotIII to SlotII to SlotI )
Start 20 RotorPosition SlotIII ! 23 RotorTurnover SlotIII !
      25 RotorPosition SlotII  !  0 RotorTurnover SlotII  !
       3 RotorPosition SlotI   !  6 RotorTurnover SlotI   !
       CycleTest
CR .slots

: one-level  ( input Rpointer Poffsetaddr -- output )
\ through a rotor
          @   \  input, Rpointer, position
     3 PICK   \  input, Rpointer, position, input
  + bounded   \  input, Rpointer, position+input
     SWAP @   \  input, position+input, rotor_address
          +   \  input, rotoraddress+position+input
        Sc@   \  input, offset
  + bounded ; \  input+offset

: A-letterY  ( letter_in -- letter_out ) \ encrypt one letter
  RotorForward SlotIII    RotorPosition SlotIII
    one-level  DUP 4 .R
  RotorForward SlotII     RotorPosition SlotII
    one-level  DUP 4 .R
  RotorForward SlotI      RotorPosition SlotI
    one-level  DUP 4 .R
  RotorForward ReflectorI RotorPosition ReflectorI
    one-level  DUP 4 .R
  RotorReverse SlotI      RotorPosition SlotI
    one-level  DUP 4 .R
  RotorReverse SlotII     RotorPosition SlotII
    one-level  DUP 4 .R
  RotorReverse SlotIII    RotorPosition SlotIII
    one-level  DUP 5 .R ;

: Increments
  \ Show the current rotor positions of Slots I, II and III
  RotorPosition SlotI   @ 3 .R   RotorPosition SlotII @ 3 .R
  RotorPosition SlotIII @ 3 .R ;

: scramble-test
  CR ."   I II III  In RIII RII RI  Re  RI RII  Out"
  #letters 0 DO
    CR Increments I 5 .R I A-letterY  ( A-letterY )
    DROP EntryComplete
  LOOP ;

CR CR .( Test 26 positions. Each value is an element output. )
CR Start  5 RotorPosition  SlotIII !
         12 RotorTurnover  SlotIII !
          5 RotorPosition  SlotII  !
          6 RotorTurnover  SlotII  !
         20 RotorPosition  SlotI   !    scramble-test

: A-letterX  ( letter_in -- letter_out ) \ encrypt one letter
  0  26 26 26 * * 0 DO
    RotorForward SlotIII    RotorPosition SlotIII    one-level
    RotorForward SlotII     RotorPosition SlotII     one-level
    RotorForward SlotI      RotorPosition SlotI      one-level
    RotorForward ReflectorI RotorPosition ReflectorI one-level
    RotorReverse SlotI      RotorPosition SlotI      one-level
    RotorReverse SlotII     RotorPosition SlotII     one-level
    RotorReverse SlotIII    RotorPosition SlotIII    one-level
    KEY? ABORT" exit"
  LOOP  .  ; ( 79 msec.!)

: ASCII>Integer   [CHAR] A - ;
: Integer>ASCII   [CHAR] A + ;

: TestAlpha  \ convert A through Z to integers
  CR [CHAR] Z 1+ [CHAR] A DO I ASCII>Integer 3 .R    LOOP
  CR                 26 0 DO I Integer>ASCII 2 SPACES EMIT
    LOOP ;
  CR  .( Numbers to letters) CR TestAlpha

: A-letter  ( letter_in -- letter_out )
  ASCII>Integer
  RotorForward SlotIII    RotorPosition SlotIII    one-level
  RotorForward SlotII     RotorPosition SlotII     one-level
  RotorForward SlotI      RotorPosition SlotI      one-level
  RotorForward ReflectorI RotorPosition ReflectorI one-level
  RotorReverse SlotI      RotorPosition SlotI      one-level
  RotorReverse SlotII     RotorPosition SlotII     one-level
  RotorReverse SlotIII    RotorPosition SlotIII    one-level
  EntryComplete
  Integer>ASCII ;

.( Message: Mister Watson Come Here I Want To See You)

CREATE Sample-In
," MISTERXWATSONXCOMEXHEREXIXWANTXTOXSEEXYOUXXXX"

CREATE Sample-Out  200 ALLOT

CREATE Sample-check 200 ALLOT

: encode ( -- ) \ encrypt a message
  Sample-Out 200 ERASE  Start
  Sample-Out   Sample-In COUNT
  0 DO  DUP    I +    C@   A-letter
        3 PICK I + 1+ C!
        I 1+ 3 PICK   C!  LOOP   2DROP ;

: decode ( -- ) \ decode sample-out into sample-check
  Sample-check 200 ERASE  Start
  Sample-check  Sample-Out COUNT
  0  DO  DUP    I +   C@    A-letter
         3 PICK I + 1+  C!
         I 1+ 3 PICK  C!  LOOP   2DROP ;

: show-formatted ( -- ) \ display as 5 letter groups
  Sample-Out COUNT  0 DO
  DUP I + C@  EMIT I 5 MOD 4 = IF BL EMIT THEN LOOP DROP ;

: show-message ( array -- ) \ display a message array
  COUNT  0 DO  DUP I + C@  EMIT  LOOP DROP ;

CR CR .( Message: Mister Watson Come Here I Want To See You )
CR CR  Sample-In  COUNT TYPE     encode
CR CR  Sample-Out COUNT TYPE
CR CR  show-formatted
       decode
CR CR Sample-check show-message


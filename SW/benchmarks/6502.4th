( A simple 6502 emulation benchmark                       cas )
( only 11 opcodes are implemented. The memory layout is:      )                   
(  2kB RAM at 0000-07FF, mirrored throughout 0800-7FFF        )
( 16kB ROM at 8000-BFFF, mirrored at C000                     )

DECIMAL
CREATE RAM 2048 ALLOT
: >RAM [ HEX ] 7FF  [ DECIMAL ] AND RAM + ;

CREATE ROM 16384 ALLOT
: >ROM [ HEX ] 3FFF [ DECIMAL ] AND ROM + ;

( 6502 registers                                              )
VARIABLE REG-A   VARIABLE REG-X   VARIABLE REG-Y
VARIABLE REG-S   VARIABLE REG-PC  : REG-PC+ REG-PC +! ;

( 6502 flags                                                  )
VARIABLE FLAG-C  VARIABLE FLAG-N  VARIABLE CYCLE
VARIABLE FLAG-Z  VARIABLE FLAG-V  : CYCLE+ CYCLE +! ;

HEX
: W@ DUP C@ SWAP 1+ C@ 100 * OR ;
: CS@ C@ DUP 80 AND IF 100 - THEN ;

: READ-BYTE ( address -- )
  DUP 8000 < IF >RAM C@ ELSE >ROM C@ THEN ;
: READ-WORD ( address -- )
  DUP 8000 < IF >RAM W@ ELSE >ROM W@ THEN ;
: DOJMP ( JMP aaaa )
  REG-PC @ >ROM W@ REG-PC ! 3 CYCLE+ ;
: DOLDA ( LDA aa )
  REG-PC @ >ROM C@ RAM + C@ DUP DUP REG-A !
  FLAG-Z ! 80 AND FLAG-N ! 1 REG-PC+ 3 CYCLE+ ;
: DOSTA ( STA aa )
  REG-A @ REG-PC @ >ROM C@ RAM + C! 1 REG-PC+ 3 CYCLE+ ;
: DOBEQ ( BEQ <aa )
  FLAG-Z @ 0= IF REG-PC @ >ROM CS@ 1+ REG-PC+ ELSE 1 REG-PC+
  THEN 3 CYCLE+ ;
: DOLDAI ( LDA #aa )
  REG-PC @ >ROM C@ DUP DUP REG-A ! FLAG-Z ! 80 AND FLAG-N !
  1 REG-PC+ 2 CYCLE+ ;
: DODEX ( DEX )
  REG-X @ 1- FF AND DUP DUP REG-X ! FLAG-Z ! 80 AND FLAG-N !
  2 CYCLE+ ;
: DODEY ( DEY )
  REG-Y @ 1- FF AND DUP DUP REG-Y ! FLAG-Z ! 80 AND FLAG-N !
  2 CYCLE+ ;
: DOINC ( INC aa )
  REG-PC @ >ROM C@ RAM + DUP C@ 1+ FF AND DUP ROT ROT SWAP C!
  DUP FLAG-Z ! 80 AND FLAG-N !  1 REG-PC+ 3 CYCLE+ ;
: DOLDY ( LDY aa )
  REG-PC @ >ROM C@ DUP DUP REG-Y ! FLAG-Z ! 80 AND FLAG-N !
  1 REG-PC+ 2 CYCLE+ ;
: DOLDX ( LDX #aa )
  REG-PC @ >ROM C@ DUP DUP REG-X ! FLAG-Z ! 80 AND FLAG-N !
  1 REG-PC+ 2 CYCLE+ ;
: DOBNE ( BNE <aa )
  FLAG-Z @ IF REG-PC @ >ROM CS@ 1+ REG-PC+ ELSE 1 REG-PC+ THEN
  3 CYCLE+ ;
: 6502EMU ( cycles -- )
  BEGIN CYCLE @ OVER  < WHILE
    REG-PC @ >ROM C@ 1 REG-PC+
    DUP 4C = IF DOJMP THEN      DUP A5 = IF DOLDA THEN
    DUP 85 = IF DOSTA THEN      DUP F0 = IF DOBEQ THEN
    DUP D0 = IF DOBNE THEN      DUP A9 = IF DOLDAI THEN
    DUP CA = IF DODEX THEN      DUP 88 = IF DODEY THEN
    DUP E6 = IF DOINC THEN      DUP A0 = IF DOLDY THEN
        A2 = IF DOLDX THEN
  REPEAT DROP ;

CREATE TESTCODE
  A9 C, 00 C,  ( start: LDA #0                                )
  85 C, 08 C,  (        STA 08                                )
  A2 C, 0A C,  (        LDX #10                               )
  A0 C, 0A C,  ( loop1: LDY #10                               )
  E6 C, 08 C,  ( loop2: INC 08                                )
  88 C,        (        DEY                                   )
  D0 C, FB C,  (        BNE loop2                             )
  CA C,        (        DEX                                   )
  D0 C, F6 C,  (        BNE loop1                             )
  4C C, 00 C, 80 C, (   JMP start                             )

: INIT-VM 13 0 DO                 ( Initialize the ROM        )
    I TESTCODE + C@ I ROM + C!
  LOOP
  0 CYCLE ! 8000 REG-PC ! ;

: BENCH6502 100 0 [ DECIMAL ] DO
    INIT-VM 6502 6502EMU [CHAR] . EMIT
  LOOP SPACE ;

\ @ 3 MHz native: 1 round: about 13m15s
\ @ 4 MHz native: 1 round: about 9m56s
\ @ 5 MHz native: 1 round: about 7m57s


\
\ tt.pfe: Tetris for terminals, redone in ANSI-Forth.
\ Written 05Apr94 by Dirk Uwe Zoller,
\ e-mail duz@roxi.rz.fht-mannheim.de.
\ Look&feel stolen from Mike Taylor's "TETRIS FOR TERMINALS"
\
\ Please copy and share this program, modify it for your system
\ and improve it as you like. But don't remove this notice.
\
\ Thank you.
\

DECIMAL  MARKER forget-tt

\ -------------------------------------------------------------
\ Z79Forth/A adaptation layer.

: 2CONSTANT
  CREATE
    SWAP , ,
  DOES>
    DUP @ SWAP CELL+ @ ;
: D= D- D0= ;
: >= < 0= ;
: off FALSE SWAP ! ;
: on  TRUE  SWAP ! ;

\ Following code block borrowed from GNU Forth 0.7.3 vt100.fs.
: pn    BASE @ SWAP DECIMAL 0 U.R BASE ! ;
: ;pn   [CHAR] ; EMIT pn ;
: ESC[  #27 EMIT [CHAR] [ EMIT ;
: AT-XY 1+ SWAP 1+ SWAP ESC[ pn ;pn [CHAR] H EMIT ;

\ A poor man's replacement for the ANS94 CASE construct.
VARIABLE _case

: CASE! _CASE ! ;
: CASE@ _CASE @ ;
: CASE? CASE@ = ;

\ -------------------------------------------------------------
\ Variables, constants

BL BL 2CONSTANT empty        \ an empty position
VARIABLE wiping              \ if true: wipe brick else draw it
 2 CONSTANT col0             \ position of the pit
 0 CONSTANT row0

10 CONSTANT wide             \ size of pit in brick positions
20 CONSTANT deep

CHAR J VALUE left-key        \ customize if you don't like them
CHAR K VALUE rot-key
CHAR L VALUE right-key
BL     VALUE drop-key
CHAR P VALUE pause-key
12     VALUE refresh-key
CHAR Q VALUE quit-key

VARIABLE score
VARIABLE pieces
VARIABLE levels
VARIABLE delay

VARIABLE brow                \ where the brick is
VARIABLE bcol

\ -------------------------------------------------------------
\ Basic random number generator

VARIABLE seed

: randomize ( -- ) 23741 seed ! ;

: random \ max -- n ; return random number < max
  seed @ 13 * $7FFF AND
  DUP seed !  SWAP MOD ;

\ -------------------------------------------------------------
\ Access pairs of characters in memory:

: 2c@ DUP 1+ C@ SWAP C@ ;
: 2c! DUP >R C! R> 1+ C! ;

: d<> D= 0=  ;  \ gForth: use 0= instead of NOT. 

\ -------------------------------------------------------------
\ Drawing primitives:

: 2emit EMIT EMIT ;

: position \ row col -- ; cursor to the position in the pit
  2* col0 + SWAP row0 + AT-XY ;

: stone \ c1 c2 -- ; draw or undraw these two characters
  wiping @ IF  2DROP 2 SPACES  ELSE  2emit  THEN ;

\ Define the pit where bricks fall into:
: def-pit
  CREATE
    wide deep * 2* ALLOT
  DOES>
    ROT wide * ROT + 2* + ;

def-pit pit

: empty-pit deep 0 DO
    wide 0 DO
      empty J I pit 2c!
    LOOP
  LOOP ;

\ Displaying:

: draw-bottom \ -- ; redraw the bottom of the pit
  deep -1 position
  [CHAR] + DUP stone
  wide 0 DO
    [CHAR] = DUP stone
  LOOP
  [CHAR] + DUP stone ;

: draw-frame \ -- ; draw the border of the pit
  deep 0 DO
    I -1   position [CHAR] | DUP stone
    I wide position [CHAR] | DUP stone
  LOOP  draw-bottom ;

\ Output a message in the bottom of the pit.
: bottom-msg \ addr cnt --
  deep OVER 2/ wide SWAP - 2/ position TYPE ;

: draw-line \ line --
  DUP 0 position
  wide 0 DO
    DUP I pit 2c@ 2emit
  LOOP  DROP ;

: draw-pit \ -- ; draw the contents of the pit
  deep 0 DO
    I draw-line
  LOOP ;

: show-key \ char -- ; visualization of that character
  DUP BL <
  IF  [CHAR] @ OR  [CHAR] ^ EMIT  EMIT  SPACE
  ELSE  [CHAR] ` EMIT  EMIT  [CHAR] ' EMIT
  THEN ;

: show-help \ -- ; display some explanations
  30  1 AT-XY ." ***** T E T R I S *****"
  30  2 AT-XY ." ======================="
  30  4 AT-XY ." Use keys:"
  32  5 AT-XY left-key     show-key ."  Move left"
  32  6 AT-XY rot-key      show-key ."  Rotate"
  32  7 AT-XY right-key    show-key ."  Move right"
  32  8 AT-XY drop-key     show-key ."  Drop"
  32  9 AT-XY pause-key    show-key ."  Pause"
  32 10 AT-XY refresh-key  show-key ."  Refresh"
  32 11 AT-XY quit-key     show-key ."  Quit"
  32 13 AT-XY ." -> "
  30 16 AT-XY ." Score:"
  30 17 AT-XY ." Pieces:"
  30 18 AT-XY ." Levels:"
  0 22 AT-XY ."  ==== This program was written 1994 in pure "
    ." dpANS Forth by Dirk Uwe Zoller ===="
  0 23 AT-XY ."  =================== Copy it, port it, play "
    ." it, enjoy it! =====================" ;

: update-score \ -- ; display current score
  38 16 AT-XY score @ 3 .R
  38 17 AT-XY pieces @ 3 .R
  38 18 AT-XY levels @ 3 .R ;

: refresh \ -- ; redraw everything on screen
  page draw-frame draw-pit show-help update-score ;

\ -------------------------------------------------------------
\ Define shapes of bricks:

: def-brick
  CREATE
    LAST COUNT EVALUATE      \ Value of the CREATEd object
    SWAP DUP >R CMOVE
    R> ALLOT
  DOES>
    ROT 4 * ROT + 2* + ;

\ Copy a temporary string living at HERE to PAD. We have to do
\ this so that the string can be used as the initial value of
\ an object created by 'def-brick'.
: >pad 2DUP PAD SWAP CMOVE NIP PAD SWAP ;

S"         ######    ##            " >pad def-brick brick1
S"         <><><><>                " >pad def-brick brick2
S"           {}{}{}  {}            " >pad def-brick brick3
S"         ()()()      ()          " >pad def-brick brick4
S"           [][]    [][]          " >pad def-brick brick5
S"         @@@@      @@@@          " >pad def-brick brick6
S"           %%%%  %%%%            " >pad def-brick brick7

\ this brick is actually in use:
S"                                 " def-brick brick

S"                                 " def-brick scratch

CREATE bricks
  ' brick1 ,  ' brick2 ,  ' brick3 ,  ' brick4 ,
  ' brick5 ,  ' brick6 ,  ' brick7 ,

CREATE brick-val
  1 C, 2 C, 3 C, 3 C, 4 C, 5 C, 5 C,

: is-brick \ brick -- ; activate a shape of brick
  >BODY ['] brick >BODY 32 CMOVE ;

: new-brick \ -- ; select a new brick at random, count it
  1 pieces +!  7 random
  bricks OVER CELLS + @ is-brick
  brick-val SWAP CHARS + C@ score +! ;

: rotleft
  4 0 DO
    4 0 DO
      J I brick 2c@  3 I - J scratch 2c!
    LOOP
  LOOP
  ['] scratch is-brick ;

: rotright
  4 0 DO
    4 0 DO
      J I brick 2c@  I 3 J - scratch 2c!
    LOOP
  LOOP
  ['] scratch is-brick ;

: draw-brick \ row col ---
  4 0 DO
    4 0 DO
      J I brick 2c@  empty d<> IF
        OVER J + over i +  position
        j i brick 2c@  stone
      THEN
    LOOP
  LOOP  2DROP ;

: show-brick wiping off draw-brick ;
: hide-brick wiping on  draw-brick ;

: put-brick \ row col -- ; put the brick into the pit
  4 0 DO
    4 0 DO
      J I brick 2c@  empty d<> IF
        OVER J +  OVER I +  pit
        J I brick 2c@  ROT 2c!
      THEN
    LOOP
  LOOP  2DROP ;

: remove-brick \ row col -- ; remove the brick from that pos.
  4 0 DO
    4 0 DO
      J I brick 2c@  empty d<> IF
        OVER J + OVER I + pit empty ROT 2c!
      THEN
    LOOP
  LOOP  2DROP ;

: test-brick \ row col -- flag ; could the brick be there?
  4 0 DO
    4 0 DO
      J I brick 2c@ empty d<> IF
        OVER J +  OVER I +
        OVER DUP 0< SWAP deep >= OR
        OVER DUP 0< SWAP wide >= OR
        2SWAP pit 2c@  empty d<>
        OR OR IF
          UNLOOP UNLOOP 2DROP FALSE  EXIT
        THEN
      THEN
    LOOP
  LOOP  2DROP TRUE ;

: move-brick \ rows cols -- flag ; try to move the brick
  brow @ bcol @ remove-brick
  SWAP brow @ + SWAP bcol @ + 2DUP test-brick IF
    brow @ bcol @ hide-brick
    2DUP bcol ! brow !  2DUP show-brick put-brick  TRUE
  ELSE
    2DROP brow @ bcol @ put-brick  FALSE
  THEN ;

: rotate-brick \ flag -- flag ; left/right, success
  brow @ bcol @ remove-brick
  DUP IF  rotright  ELSE  rotleft  THEN
  brow @ bcol @ test-brick
  OVER IF  rotleft  ELSE  rotright  THEN
  IF
    brow @ bcol @ hide-brick
    IF  rotright  ELSE  rotleft  THEN
    brow @ bcol @ put-brick
    brow @ bcol @ show-brick  TRUE
  ELSE
    DROP FALSE
  THEN ;

: insert-brick \ row col -- flag ; introduce a new brick
  2DUP test-brick if
    2DUP bcol ! brow !
    2DUP put-brick  draw-brick  TRUE
  ELSE
    FALSE
  THEN ;

: drop-brick \ -- ; move brick down fast
  BEGIN
    1 0 move-brick 0=
  UNTIL ;

: move-line \ from to --
  OVER 0 pit  OVER 0 pit  wide 2*  CMOVE  draw-line
  DUP 0 pit  wide 2*  BLANK  draw-line ;

: line-full \ line-no -- flag
  TRUE  wide 0 DO
    OVER I pit 2c@ empty D= IF
      DROP FALSE  LEAVE
    THEN
  LOOP NIP ;

: remove-lines \ --
  deep deep
  BEGIN
    SWAP
    BEGIN
      1- DUP 0< IF  2DROP EXIT  THEN
      DUP line-full
    WHILE
      1 levels +!  10 score +!
    REPEAT
    SWAP 1-
    2DUP <> IF  2DUP move-line  THEN
  AGAIN ;

: to-upper \ char -- char ; convert to upper case
  DUP [CHAR] a [CHAR] z 1+ WITHIN IF
    BL -
  THEN ;

: interaction \ -- flag
  key to-upper CASE!
  left-key     CASE?  IF  0 -1 move-brick DROP  THEN
  right-key    CASE?  IF  0  1 move-brick DROP  THEN
  rot-key      CASE?  IF  0 rotate-brick DROP  THEN
  drop-key     CASE?  IF  drop-brick  THEN
  pause-key    CASE?  IF  S"  paused " bottom-msg  KEY DROP
                        draw-bottom  THEN
  refresh-key  CASE?  IF  refresh  THEN
  quit-key     CASE?  IF  FALSE EXIT  THEN
  TRUE ;

: initialize \ -- ; prepare for playing
  empty-pit refresh
  0 score !  0 pieces !  0 levels !  100 delay ! ;

: adjust-delay \ -- ; make it faster with increasing score
  levels @
  DUP  50 < IF  100 OVER -  ELSE
  DUP 100 < IF   62 OVER 4 / -  ELSE
  DUP 500 < IF   31 OVER 16 / -  ELSE  0  THEN
  THEN THEN
  delay !  DROP ;

: empty-stack BEGIN DEPTH WHILE DROP REPEAT ;

\ -------------------------------------------------------------
\ Game engine.

: play-game \ -- ; play one tetris game
  BEGIN
    new-brick
    -1 3 insert-brick
  WHILE
    BEGIN
      4 0 DO
        35 13 AT-XY
        delay @ MS KEY? IF
          interaction 0= IF  UNLOOP EXIT  THEN
        THEN
      LOOP
      1 0 move-brick  0=
    UNTIL
    remove-lines
    update-score
    adjust-delay
  REPEAT ;

: tt \ -- ; play the tetris game
  randomize initialize
  S"  Press any key " bottom-msg key drop draw-bottom
  BEGIN
    play-game
    S"  Again? " bottom-msg KEY to-upper [CHAR] Y =
  WHILE
    initialize
  REPEAT
  empty-stack
  0 23 AT-XY CR ;

\ -------------------------------------------------------------
\ Main entry point.

tt forget-tt


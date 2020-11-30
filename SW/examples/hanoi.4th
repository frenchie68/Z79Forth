( Towers of Hanoi. Original code by Peter Midnight.           )
( Forth Dimensions, II/2. July/August 1980, page 32.          )
( Z79Forth adaptations, FLA, 07/14/2020.                      )

DECIMAL
: H.MARKER ;
CHAR 0 CONSTANT H.ASC.0
    27 CONSTANT H.ASC.ESC

( Control Sequence Introducer                                 )
: H.VT100.CSI ( -- ) H.ASC.ESC EMIT [CHAR] [ EMIT ;
( Numeric parameter. Convert the TOS to a string and print it )
: H.VT100.NP ( n -- ) 10 /MOD H.ASC.0 + EMIT H.ASC.0 + EMIT ;

( Both X and Y have a lower bound of zero for this use case.  )
: GOTOXY ( X Y -- ) 1+ SWAP 1+ SWAP H.VT100.CSI
  H.VT100.NP [CHAR] ; EMIT
  H.VT100.NP [CHAR] H EMIT ;

: 4DUP 2OVER 2OVER ;
10 CONSTANT NMAX                    ( Maximum number of rings )
VARIABLE (N)  NMAX (N) !  : N (N) @ ;

CHAR + CONSTANT COLOR
CREATE RING  N 2+  ALLOT               ( array[1..N] of bytes )
  RING NMAX 2+ 0 FILL

: DELAY ( centiseconds -- ) 10 * MS ;
: HWAIT 10 MS ;
: POS ( location pos -- coordinate )
  N 2* 1+ *  N + ;
: HALFDISPLAY ( color size -- ) 0 DO
    DUP EMIT
  LOOP DROP ;
: <DISPLAY> ( line color size -- )
  2DUP HALFDISPLAY  ROT 3 <  IF BL ELSE [CHAR] | THEN
  EMIT  HALFDISPLAY ;
: DISPLAY ( size pos line color -- )
  SWAP >R -ROT OVER - R@           ( color\size\pos-size\line )
  GOTOXY  R> ( color\size\line )  -ROT <DISPLAY> ;

: PRESENCE ( tower ring -- flag )
  RING +  C@  = ;
: LINE ( tower line -- display_line_of_top )
  4 SWAP  N 0 DO
    DUP I PRESENCE 0= ( ABS ) ROT + SWAP
  LOOP DROP ;

: RAISE ( size tower -- )
  DUP  POS  SWAP  LINE  2  SWAP  DO
    2DUP I BL DISPLAY  2DUP  I 1- COLOR DISPLAY
  -1 +LOOP  2DROP ;

: LOWER ( size tower -- )
  DUP  POS  SWAP  LINE 1+ 2 DO
    2DUP  I 1-  BL DISPLAY
    2DUP I COLOR DISPLAY
  LOOP 2DROP ;  

: MOVELEFT ( size source_tower dest_tower -- )
  POS   SWAP  POS 1-  DO
    DUP I 1+ 1 BL DISPLAY
    DUP I 1 COLOR DISPLAY
    HWAIT
  -1 +LOOP  DROP ;
: MOVERIGHT ( size source_tower dest_tower -- )
  POS 1+  SWAP  POS 1+  DO
    DUP I 1- 1 BL DISPLAY
    DUP I 1 COLOR DISPLAY
    HWAIT
  LOOP DROP ;
: TRAVERSE ( size source_tower dest_tower -- )
  2DUP >  IF MOVELEFT ELSE MOVERIGHT THEN ;
: MOVE ( size source_tower dest_tower -- )
  -ROT 2DUP RAISE  >R 2DUP R> ROT TRAVERSE
  2DUP  RING + 1- C!  SWAP LOWER ;

: MULTIMOV ( size source dest spare -- )
  4 PICK  1 =  IF
    DROP MOVE
  ELSE
    >R >R SWAP 1- SWAP R> R>
    4DUP SWAP RECURSE
    4DUP DROP  ROT 1+ -ROT  MOVE
    -ROT SWAP  RECURSE
  THEN ;
: MAKETOWER ( tower -- )
  POS  4 N +  3 DO
    DUP I GOTOXY  [CHAR] | EMIT
  LOOP  DROP ;
: MAKEBASE ( -- )
  0 N 4 + GOTOXY  N 6 * 3 +  0 DO
    [CHAR] - EMIT
  LOOP ;
: MAKERING ( tower size -- )
  2DUP RING + 1- C!
  SWAP LOWER ;
: SETUP ( -- ) PAGE
  N 1+ 0 DO  1 RING I + C!  LOOP
  3 0 DO  I MAKETOWER  LOOP
  MAKEBASE
  1 N DO  0 I MAKERING  -1 +LOOP ;

: TOWERS ( rings_count -- )
  1 MAX  NMAX MIN  (N) !
  SETUP  N 2 0 1 BEGIN
    OVER POS  N 4 +  GOTOXY  N 0 DO  50 DELAY  LOOP
    ROT  4DUP  MULTIMOV
    KEY?
  UNTIL KEY DROP
  2DROP 2DROP ;


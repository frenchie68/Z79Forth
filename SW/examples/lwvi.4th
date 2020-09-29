( This targets Minicom in VT102 mode with BS mapped to BS     )
( The ultimate ref. is at https://vt100.net/docs/vt102-ug/    )
( In replace mode safe Minicom settings are: ^AZTD 20 ^AZTF 2 )
( In insert mode: ^AZTD 80 ^AZTF 8 )
DECIMAL
: E.MARKER ;  ( Allow editor's code memory to be disposed of )

( GNU Forth specific material )
( : BLANKS BL FILL ; )
( : 2+ 2 + ; )
( : 1+! DUP @ 1+ SWAP ! ; )
( : -! SWAP NEGATE SWAP +! ; )
( : NOT 0= ; )
( : FIND ' ;    ( This overrides the ANSI FIND... )
( : E.64* 64 * ; )
( : UNLESS ['] 0= COMPILE, POSTPONE IF ; IMMEDIATE RESTRICT )
( : MONITOR ; )

( Z79Forth specific material )
: E.64* 6 SHIFT ;

( Generic part of the implementation )
CHAR 0 CONSTANT E.ASC.0
CHAR $ CONSTANT E.ASC.$
    27 CONSTANT E.ASC.ESC
     8 CONSTANT E.ASC.BS
     9 CONSTANT E.ASC.TAB
    13 CONSTANT E.ASC.CR   ( aka CTRL-M )

( Control Sequence Introducer )
: E.VT100.CSI ( -- ) E.ASC.ESC EMIT [CHAR] [ EMIT ;
( Numeric parameter. Convert the TOS to a string and print it )
: E.VT100.NP ( n -- ) 10 /MOD E.ASC.0 + EMIT E.ASC.0 + EMIT ;

: E.VT100-1PCMD CREATE ( vt100.opcode -- ) ,
  DOES> ( vt100.param -- ) E.VT100.CSI SWAP E.VT100.NP @ EMIT ;
( Cursor Backward )   CHAR D E.VT100-1PCMD E.VT100.CUB  MONITOR
( Cursor Forward )    CHAR C E.VT100-1PCMD E.VT100.CUF  MONITOR
( Cursor Down )       CHAR B E.VT100-1PCMD E.VT100.CUD  MONITOR
( Cursor Up )         CHAR A E.VT100-1PCMD E.VT100.CUU  MONITOR
( Ins/Repl Mode ins ) CHAR h E.VT100-1PCMD E.VT100.IRMI MONITOR
( Ins/Repl Mode rep ) CHAR l E.VT100-1PCMD E.VT100.IRMR MONITOR
( Delete Character )  CHAR P E.VT100-1PCMD E.VT100.DCH  MONITOR
( Insert Line      )  CHAR L E.VT100-1PCMD E.VT100.IL   MONITOR
( Delete Line      )  CHAR M E.VT100-1PCMD E.VT100.DL   MONITOR
( Erase in Display )  CHAR J E.VT100-1PCMD E.VT100.ED   MONITOR
( Cursor Position, aka gotoxy. )
: E.VT100.CUP ( x y -- ) E.VT100.CSI
  E.VT100.NP [CHAR] ; EMIT E.VT100.NP [CHAR] H EMIT ;

: E.VT100.BS 1 E.VT100.CUB ;
: E.VT100.INS 4 E.VT100.IRMI ; ( VT100 insert mode )
: E.VT100.RPL 4 E.VT100.IRMR ; ( VT100 replace mode )
: E.VT100.ED$ 0 E.VT100.ED ;   ( VT100 delete to end of screen )

VARIABLE E.MODE ( 0: COMMAND, 1: INSERT, 2: REPLACE, 3: QUIT  )
VARIABLE E.COL#  VARIABLE E.LIN#
VARIABLE E.COL#.IRM.ENTERED
VARIABLE E.CMD.COUNT VARIABLE E.CMD.SUBCOUNT
VARIABLE E.MINICOM.BUG.WORKAROUND

: SCR1 SCR @ BLOCK ;
: WIPE ( -- ) SCR1 1024 BLANKS UPDATE FLUSH ;

: E.ISDIGIT ( n -- f ) E.ASC.0 [CHAR] 9 1+ WITHIN ;
: E.CMD.COUNT.CLR 0 E.CMD.COUNT ! ;
: E.CMD.COUNT.GET E.CMD.COUNT @ ?DUP UNLESS 1 THEN ;
: E.CMD.SUBCOUNT.CLR 0 E.CMD.SUBCOUNT ! ;
: E.CMD.SUBCOUNT.GET E.CMD.SUBCOUNT @ ?DUP UNLESS 1 THEN ;

( All cursor positioning primitives accept a cmd count prefix )
: E.CUF E.CMD.COUNT.GET 0 DO                 ( Cursor forward )
    E.COL# @ 63 <> IF E.COL# 1+!  1 E.VT100.CUF THEN
  LOOP ;
: E.CUB E.CMD.COUNT.GET 0 DO                ( Cursor backward )
    E.COL# @ IF 1 E.COL# -! 1  E.VT100.CUB THEN
  LOOP ;
: E.CUD E.CMD.COUNT.GET 0 DO                    ( Cursor down )
    E.LIN# @ 15 <> IF E.LIN# 1+!  1 E.VT100.CUD THEN
  LOOP ;
: E.CUU E.CMD.COUNT.GET 0 DO                      ( Cursor up )
    E.LIN# @ IF 1 E.LIN# -!  1 E.VT100.CUU THEN
  LOOP ;

: E.KEY.IS.ESC KEY DUP E.ASC.ESC = ;
: E.CUR.REFRESH ( -- ) E.COL# @  1+ E.LIN# @ 2+  E.VT100.CUP ;

( This primitive leaves the terminal in replace mode )
: E.CUR.STATUS.CUP ( -- ) 1 18 E.VT100.CUP  E.VT100.RPL ;
( x, y are expressed in memory space, i.e. starting at 0 )
: E.SCR-ADDR.GET ( x y -- addr ) E.64* + SCR1 + ;
: E.CUR-ADDR.GET ( -- addr ) E.COL# @ E.LIN# @ E.SCR-ADDR.GET ;

: E.COL.IRM.ENTERED.SET E.COL# @ E.COL#.IRM.ENTERED ! ;

: E.SW2CMD ( -- ) 0 E.MODE ! E.CMD.COUNT.CLR
  E.CUR.STATUS.CUP  E.VT100.ED$  E.CUR.REFRESH ;
: E.SW2INS ( -- ) 1 E.MODE !  E.CUR.STATUS.CUP
  ." -- INSERT -- "  E.VT100.ED$
  E.CUR.REFRESH E.VT100.INS  E.COL.IRM.ENTERED.SET
  1 E.MINICOM.BUG.WORKAROUND ! ;
: E.SW2RPL ( -- ) 2 E.MODE !  E.CUR.STATUS.CUP
  ." -- REPLACE --"  E.VT100.ED$
  E.CUR.REFRESH  E.COL.IRM.ENTERED.SET ;
: E.SW2QUT ( -- ) 3 E.MODE !  E.VT100.RPL  67 18 E.VT100.CUP ;

( This is required by E.SITREP et al )
: E.MODE.REFRESH ( -- ) E.MODE @ >R
  I 0=  IF E.SW2CMD ELSE
  I 1 = IF E.SW2INS ELSE
  I 2 = IF E.SW2RPL
  THEN THEN THEN R> DROP ;

: E.CURLINE.REDRAW 0 E.LIN# @  OVER 1+ OVER 2+ E.VT100.CUP
  E.SCR-ADDR.GET  64 TYPE  E.CUR.REFRESH ;

( This might looks overly complex but it has been reworked )
( from its original simplicity to support the 'd' functions )
: E.L/C.FROM-ADDR.SET ( addr -- ) SCR1 - 64 /MOD
  E.LIN# !  E.COL# ! ;
: E.L/C.FROM-ADDR.CUP ( addr -- )
  E.L/C.FROM-ADDR.SET  E.CUR.REFRESH ;
: E.SOL.ADDR.GET ( -- addr ) 0 E.LIN# @ E.SCR-ADDR.GET ;
: E.SOL ( -- ) E.SOL.ADDR.GET  E.L/C.FROM-ADDR.CUP ;
: E.EOL.ADDR.GET ( -- addr ) 0 E.LIN# @ E.SCR-ADDR.GET
  0 63 DO
    DUP I + C@ BL <> IF I + LEAVE THEN
  -1 +LOOP ;
: E.EOL ( -- ) E.EOL.ADDR.GET  E.L/C.FROM-ADDR.CUP ;

( Delete current character, in the context of a 64 char line )
( We do insert a BL character at column #63 )
: E.DCH.1CHAR 63 E.COL# @ - ?DUP IF
    E.CUR-ADDR.GET   ( Destination address )
    DUP 1+ SWAP      ( Source address )
    ROT CMOVE
  THEN
  1 E.VT100.DCH
  BL 63 E.LIN# @ E.SCR-ADDR.GET C! ;
: E.DCH ( -- ) E.CMD.COUNT.GET 0 DO
    E.DCH.1CHAR
  LOOP ;
: E.DCH.BACK E.CMD.COUNT.GET 0 DO
    E.COL# @ IF
      1 E.COL# -! E.VT100.BS
      E.DCH.1CHAR
    THEN
  LOOP ;

: E.RPL.1CHAR ( -- ) KEY DUP E.ASC.TAB = IF DROP EXIT THEN
  E.CMD.COUNT.GET 0 DO
    E.COL# @ 64 = IF
      LEAVE
    ELSE
      DUP DUP E.CUR-ADDR.GET C!  EMIT  E.COL# 1+!
    THEN
  LOOP  DROP  1 E.COL# -!  E.VT100.BS ;

: E.APPEND ( -- ) E.COL# @ 63 = IF EXIT THEN 
  E.CUF  E.SW2INS ;

( Entered in command mode--VT100 RPL--and left the same way )
: E.SITREP ( -- ) ( Invoked on ^G ) E.CUR.STATUS.CUP
  ." Lin " E.LIN# @ 1+ . ." Col " E.COL# @ 1+ . ." Blk " SCR ?
  1000 MS  E.MODE.REFRESH  E.CUR.REFRESH ;

( VIM has a transactional behavior with respect to the )
( command count, i.e. 3fO will leave the cursor position )
( unchanged if three occurrences of 'O' cannot be found )
( on the current line. The same semantics is implemented here )
( The search begins one character after the current column )
: E.CHAR.FIND ( -- ) E.COL# @ 63 < UNLESS EXIT THEN
  KEY >R   ( the character to be looked for )
  E.CMD.COUNT.GET
  64 E.COL# @ 1+ DO
    I E.LIN# @ E.SCR-ADDR.GET C@ J = IF
      1- DUP UNLESS
        I E.COL# !  E.CUR.REFRESH  LEAVE
      THEN
    THEN 
  LOOP
  R> 2DROP ;

( w-ord and b-ack accept a command count and operate across )
( multiple lines. Which is problematic when it comes to )
( implement d<n>w. In a regular VI implementation that might )
( imply a join operation!!! )
: E.WORD.BOUNDARY.FOUND ( addr -- ) SCR1 - 64 /MOD
  E.LIN# !  E.COL# !  E.CUR.REFRESH ;
: E.WORD.BOUNDARY.FIND ( ccnt chrflg addr -- ccnt chrflg )
  >R  DUP BL = IF ( Looking for non-space char @ addr )
    I C@ BL <> IF
      DROP [CHAR] X ( set chrflg to non-BL )
      SWAP 1- ( Decrement ccnt ) SWAP
    THEN
  ELSE ( Looking for space and skipping all non-spaces )
    I C@ BL = IF
      DROP BL ( set chrflg to BL )
    THEN
  THEN R> DROP ;
: E.WORD.INIT >R ( dir -- cmdcount chrflg maxaddr startaddr )
  E.CMD.COUNT.GET
  ( A kludge for searching backward for a word beginning )
  I 0< IF
    E.CUR-ADDR.GET C@ BL <> IF
      E.CUR-ADDR.GET SCR1 <> IF
        E.CUR-ADDR.GET 1- C@ BL = IF
          1+
  THEN THEN THEN THEN
  E.CUR-ADDR.GET DUP C@ SWAP
  SCR1 I 0> IF 1024 + THEN
  SWAP R> DROP ;
: E.WORD.BCK ( -- ) -1 E.WORD.INIT DO
    OVER 0= UNLESS I E.WORD.BOUNDARY.FIND THEN
    OVER 2 < ( cmdcount is <= 1 )
    OVER BL <> AND  ( and we're looking for space ) IF
      ( If I=I' we're done else iterate until char[i-1] is BL )
      I I' = IF
        I E.WORD.BOUNDARY.FOUND LEAVE
      ELSE I 1- C@ BL = IF
        I E.WORD.BOUNDARY.FOUND LEAVE
      THEN THEN
    THEN
  -1 +LOOP 2DROP ;
: E.WORD.FWD-ADDR.GET ( -- addr ) 0 ( default address returned )
  1 E.WORD.INIT DO
    I E.WORD.BOUNDARY.FIND
    OVER 0= IF ROT DROP I -ROT LEAVE THEN
  LOOP 2DROP ;
: E.WORD.FWD ( -- ) E.WORD.FWD-ADDR.GET
  ?DUP IF E.WORD.BOUNDARY.FOUND THEN ;

: E.DELETE ( -- ) E.CMD.SUBCOUNT.CLR
  E.KEY.IS.ESC IF DROP EXIT THEN
  DUP E.ASC.0 = IF
    E.CUR-ADDR.GET E.SOL.ADDR.GET - E.CMD.COUNT !  E.DCH.BACK
  ELSE
    DUP E.ASC.$ = IF
      E.EOL.ADDR.GET E.CUR-ADDR.GET 2DUP < UNLESS
        - 1+ E.CMD.COUNT !  E.DCH
      ELSE
        2DROP
      THEN
    ELSE ( 'd' command to be added at this level )
      BEGIN
        DUP E.ISDIGIT
      WHILE
        E.ASC.0 - E.CMD.SUBCOUNT @ 10 * + E.CMD.SUBCOUNT !
        KEY
      REPEAT
      E.CMD.SUBCOUNT.GET E.CMD.COUNT.GET * E.CMD.COUNT !
      ( At this point we expect 'f'+<char> or 'w' )
      DUP [CHAR] w = IF
        E.WORD.FWD-ADDR.GET ?DUP IF
          E.CUR-ADDR.GET - E.CMD.COUNT !  E.DCH
        THEN
      THEN
    THEN
  THEN DROP ;

: E.IS.BLANK.LINE ( lineno -- flag ) E.64* SCR1 +
  1 ( "a priori" return value ) 64 0 DO
    OVER I + C@ BL <> IF DROP 0 LEAVE THEN
  LOOP NIP ;
: E.LAST.BLANK.LINE.GO ( -- )
  0 15 DO
    I E.IS.BLANK.LINE IF
      I E.LIN# !  0 E.COL# !
    ELSE
      LEAVE
    THEN
  -1 +LOOP ;
( If no command count is supplied, point to the last blank )
( line of the block )
: E.LINE.GO ( -- ) E.CMD.COUNT @ DUP 1 17 WITHIN IF
    1- E.LIN# !  0 E.COL# !
  ELSE
    UNLESS E.LAST.BLANK.LINE.GO THEN
  THEN E.CUR.REFRESH ;

: E.LINE.FIRST-NONSPC-COL.GO ( -- ) 0 DUP E.COL# !
    E.LIN# @ E.SCR-ADDR.GET >R ( SOL address )
  64 0 DO
    I J + C@ BL <> IF I E.COL# !  LEAVE  THEN
  LOOP
  R> DROP E.CUR.REFRESH ;
: E.LINE.UP ( -- ) E.CMD.COUNT.GET 0 DO
    E.LIN# @ IF
      1 E.LIN# -!  E.LINE.FIRST-NONSPC-COL.GO
    THEN
  LOOP ;
: E.LINE.DOWN ( -- ) E.CMD.COUNT.GET 0 DO
    E.LIN# @ 15 <> IF
      E.LIN# 1+!  E.LINE.FIRST-NONSPC-COL.GO
    THEN
  LOOP ;

( An associative array of sorts )
( A poor man's replacement for a CASE primitive )
CREATE E.CMD.JUMPTBL
CHAR l , FIND E.CUF ,          CHAR h , FIND E.CUB ,
CHAR j , FIND E.CUD ,          CHAR k , FIND E.CUU ,
CHAR i , FIND E.SW2INS ,       CHAR R , FIND E.SW2RPL ,
CHAR 0 , FIND E.SOL ,          CHAR $ , FIND E.EOL ,
CHAR x , FIND E.DCH ,          CHAR r , FIND E.RPL.1CHAR ,
CHAR a , FIND E.APPEND ,       7      , FIND E.SITREP ,
CHAR X , FIND E.DCH.BACK ,     CHAR f , FIND E.CHAR.FIND ,
CHAR w , FIND E.WORD.FWD ,     CHAR W , FIND E.WORD.FWD ,
CHAR b , FIND E.WORD.BCK ,     CHAR B , FIND E.WORD.BCK ,
CHAR G , FIND E.LINE.GO ,      E.ASC.CR , FIND E.LINE.DOWN ,
CHAR + , FIND E.LINE.DOWN ,    CHAR - , FIND E.LINE.UP ,
CHAR d , FIND E.DELETE ,
0      ,             MONITOR   ( End of table marker )

: E.COMMAND ( -- ) KEY DUP [CHAR] : = IF ( : handling )
    DROP KEY [CHAR] q = IF E.SW2QUT KEY DROP THEN EXIT THEN
  ( 0 is not considered digit count unless E.CMD.COUNT is NZ )
  DUP E.ASC.0 = E.CMD.COUNT @ 0= AND UNLESS
    DUP E.ISDIGIT IF        ( Update the command count prefix )
      E.ASC.0 - E.CMD.COUNT @ 10 * + E.CMD.COUNT ! EXIT
    THEN
  THEN
  E.CMD.JUMPTBL BEGIN
    DUP @ 0= IF 2DROP E.CMD.COUNT.CLR EXIT THEN
    2DUP @ = IF NIP 1 CELLS + @ EXECUTE E.CMD.COUNT.CLR EXIT
    THEN
    2 CELLS +
  AGAIN ;

: E.INS.CR.HANDLE ( -- ) E.LIN# @ 15 = IF EXIT THEN
  0 E.LIN# @ 1+ E.SCR-ADDR.GET
    DUP 64 +
    14 E.LIN# @ - E.64*
    CMOVE>
  E.CUR-ADDR.GET
    0 E.LIN# @ 1+ E.SCR-ADDR.GET DUP >R
    64 E.COL# @ - DUP >R
    CMOVE
  E.CUR-ADDR.GET I BLANKS
  I' I + E.COL# @ BLANKS
  R> E.VT100.DCH R> DROP
  0 E.COL# ! E.LIN# 1+!
  E.CUR.STATUS.CUP E.VT100.ED$
  E.CUR.REFRESH 1 E.VT100.IL E.CURLINE.REDRAW
  E.MODE.REFRESH E.CUR.REFRESH ;

: E.INS.BS.HANDLE ( -- )
  E.COL# @ E.COL#.IRM.ENTERED @ = IF EXIT THEN
  E.VT100.RPL  E.VT100.BS  1 E.COL# -!  E.DCH.1CHAR
  E.VT100.INS ;

( This is insert the block way. Insertions are performed on   )
( the same line, loosing characters beyond column #63.        )
: E.INS.1CHAR.HANDLE ( c -- )
  DUP E.ASC.TAB = IF DROP EXIT THEN
  63 E.LIN# @ E.SCR-ADDR.GET C@ BL <> IF
    ( Need to blank character at column #63 ) E.VT100.RPL
    64 E.LIN# @ 2+ E.VT100.CUP  SPACE  E.CUR.REFRESH
    E.VT100.INS
  THEN
  DUP E.CUR-ADDR.GET
    DUP  DUP 1+  63 E.COL# @ -  CMOVE>
  C!  E.MINICOM.BUG.WORKAROUND @ IF
    DROP  0 E.MINICOM.BUG.WORKAROUND !
    E.VT100.RPL  E.CURLINE.REDRAW  E.VT100.INS  1 E.VT100.CUF
  ELSE
    EMIT
  THEN
  E.COL# @ 63 = IF ( Point to next line or allow Col #64? )
    E.VT100.BS  EXIT
  THEN  E.COL# 1+! ;

( We wrap from col #63 to the first col on the next line )
: E.RPL.1CHAR.HANDLE ( c -- ) DUP E.ASC.TAB = IF DROP EXIT THEN
  DUP E.CUR-ADDR.GET C!  EMIT
  E.COL# @ 63 <> IF
    E.COL# 1+!
  ELSE E.LIN# @ 15 <> IF
      0 E.COL# !  E.LIN# 1+!  E.CUR.REFRESH
    ELSE
      E.VT100.BS
    THEN
  THEN ;

: E.TO-EOL.BLANK ( -- )
  64 E.COL# @ - E.CUR-ADDR.GET OVER BLANKS  SPACES ;
: E.RPL.CR.HANDLE ( -- ) E.TO-EOL.BLANK
  E.LIN# @ 15 <> IF
    0 E.COL# !  E.LIN# 1+!  E.TO-EOL.BLANK
    E.COL.IRM.ENTERED.SET
  THEN E.CUR.REFRESH ;
: E.RPL.BS.HANDLE ( -- ) E.COL# @ E.COL#.IRM.ENTERED @ =
  IF EXIT THEN
  E.VT100.BS  SPACE  E.VT100.BS
  1 E.COL# -!  BL E.CUR-ADDR.GET C! ;

: E.INSERT ( -- ) E.KEY.IS.ESC IF DROP E.SW2CMD EXIT THEN
  DUP E.ASC.CR = IF DROP E.INS.CR.HANDLE EXIT THEN
  DUP E.ASC.BS = IF DROP E.INS.BS.HANDLE EXIT THEN
  E.INS.1CHAR.HANDLE ;

: E.REPLACE ( -- ) E.KEY.IS.ESC IF DROP E.SW2CMD EXIT THEN
  DUP E.ASC.CR = IF DROP E.RPL.CR.HANDLE EXIT THEN
  DUP E.ASC.BS = IF DROP E.RPL.BS.HANDLE EXIT THEN
  E.RPL.1CHAR.HANDLE ;

: E.VARS.INIT 0 DUP DUP E.COL# ! E.LIN# !
  E.MINICOM.BUG.WORKAROUND ! ;

( Lightweight VI main entry point )
: E.LWVI ( screen -- ) PAGE
  LIST  ( LIST will consumme screen and set SCR's value )
  E.VARS.INIT  E.SW2CMD  E.CUR.REFRESH
  BEGIN E.MODE @ >R
    I 0=  IF E.COMMAND ELSE
    I 1 = IF E.INSERT  ELSE
    I 2 = IF E.REPLACE
    THEN THEN THEN R> DROP
    E.MODE @ 3 = IF UPDATE  FLUSH  EXIT THEN
  AGAIN ;
: EDIT ['] E.LWVI EXECUTE ;


\ F+L Variables and pointers

\ Variables and constants.
VARIABLE COLUMN   \ Current column pointer.
VARIABLE TAB      \ Position of left margin,
VARIABLE #LINES   \ Count of lines since last FF.
VARIABLE FND      \ Flag indicating currently parsed word was
                  \ found to be a special formatting word.
VARIABLE IX       \ Temporary storage for the value of >IN.

79 CONSTANT LINE-SIZE \ Column width of output device.
65 CONSTANT MAX-LINES \ Lines per page of output device.
 2 CONSTANT OFFSET    \ Size of indentations in columns.
34 CONSTANT A"        \ ASCII value of double quote.
41 CONSTANT RPAREN    \ ASCI value of right parenthesis.

' \ CONSTANT BSLASH
' ( CONSTANT PAREN

\ F+L Arrays of pointers to formatting words.
CREATE QUOTES  1 , ' ."     ,
CREATE STARTS  1 , ' :      ,
CREATE ENDS    1 , ' ;      ,
CREATE OUTS    5 , ' UNTIL  , ' LOOP   ,
                   ' +LOOP  , ' THEN   ,
                   ' REPEAT ,
CREATE INS     4 , ' IF     , ' DO     ,
                   ' BEGIN  ,
                   ' UNLESS ,
CREATE IN+OUTS 3 , ' ELSE   , ' WHILE  ,
                   ' DOES>  ,

\ F+L SFF >= BELL CR? INDENT OUTDENT NEW-LINE
\ -- Perform a carriage return (maybe) and a form feed (maybe).
\    Don't do CR on new lines. This avoids blank lines.
\ -- Emit a form feed.
: SFF ;  : >= < 0= ;  : BELL ;
: CR? COLUMN @ TAB @ > IF       \ Legitimate CR?
    CR #LINES @ MAX-LINES = IF  \ Do CR, need a FF?
      SFF 0 #LINES !            \ Do FF
    ELSE
      1 #LINES +!               \ Count lines
    THEN
    0 COLUMN !
  THEN ;                        \ Reset column pointer.
\ n -- Indent by constant OFFSET times n.
: INDENT CR? OFFSET * TAB +! ;
\ n -- Move margin left OFFSET times n. Don't move off page.
: OUTDENT CR? TAB @ SWAP OFFSET * - DUP 0<
  IF BELL THEN 0 MAX TAB ! ;
\ -- Reset left margin and invoke carriage return test.
: NEW-LINE 0 TAB ! CR? ;

\ F+L DONE? PARSE TYPE-IT
\ -- fl Check for end of screen.
: DONE? >IN @ 1023 >= ;

\ -- Place next word at HERE ready for typing.
: PARSE IX @ >IN ! BL WORD DROP ;

\ -- Type one word, first do CR if no room on this line.
: <TYPE> COLUMN @ TAB @ < IF
    TAB @ COLUMN @ -  SPACES    \ Insert leading spaces
    TAB @ COLUMN !              \ Set new margin
  THEN
  HERE COUNT 1+                 \ str len+1 (includes delim)
  DUP COLUMN @ + LINE-SIZE >
  IF
    CR?  TAB @ SPACES  TAB @ COLUMN !
  THEN
  DUP COLUMN +!
  TYPE
  -1 FND ! ;
\ --
: TYPE-IT PARSE <TYPE> ;

\ F+L ?? F+L-INIT TYPE-TO- TYPE-TO-END-OF-LINE
\ x y -- x fl Flag true if x found in array at address y.
: ?? 0 SWAP DUP                 \ x\0\y\y
  @ 0  ( x\0\y\nelts\0 ) DO     \ x\0\y
    CELL+ DUP @ 4 PICK =        \ x\0\y'
    IF
      SWAP 1+ SWAP LEAVE        \ x\1\y'
    THEN
  LOOP DROP ;                   \ x\f

\ n -- m p List block n. Save BLK and >IN, restore later.
: F+L-INIT DUP BLOCK DROP       \ Map in block #n
  BLK @ >IN @                   \ Backup current BLK and >IN
  ROT BLK ! 0 >IN !             \ Have BLK/>IN point to target
  1 #LINES ! 0 TAB ! 0 COLUMN ! \ Initialize essential vars
  ." Screen #" BLK ? CR ;

\ n -- Type text till next occurrence of n.
: TYPE-TO- CR?  TYPE-IT
  WORD C@ 1+ HERE C!
  BL HERE DUP C@
  1+ + C! <TYPE> CR? ;

\ -- Type comment line, round up >IN.
: TYPE-TO-END-OF-LINE CR? TYPE-IT
  BLK @ BLOCK >IN @ +
    >IN @ 63 INVERT AND 64 +
  DUP >R >IN @ - -TRAILING TYPE CR? R> >IN ! ;

\ F+L END
: F+L ( screenno -- ) F+L-INIT BEGIN
    0 FND !  >IN @ IX !  BL WORD DROP  DONE? NOT
  WHILE
    IX @ >IN !  '
    QUOTES   ??      IF      A"   TYPE-TO-                THEN
    DUP PAREN =      IF  RPAREN   TYPE-TO-                THEN
    DUP BSLASH =     IF           TYPE-TO-END-OF-LINE     THEN
    STARTS   ??      IF NEW-LINE  TYPE-IT >IN @ IX !
                                  TYPE-IT 2 INDENT        THEN
    INS      ??      IF 1 INDENT  TYPE-IT 1 INDENT        THEN
    OUTS     ??      IF 1 OUTDENT TYPE-IT 1 OUTDENT       THEN
    ENDS     ??      IF 2 OUTDENT TYPE-IT CR?             THEN
    IN+OUTS  ??      IF 1 OUTDENT TYPE-IT 1 INDENT        THEN
    DROP  FND @  UNLESS           TYPE-IT                 THEN
  REPEAT CR SFF   >IN ! BLK ! ;   \ Restore initial >IN and BLK

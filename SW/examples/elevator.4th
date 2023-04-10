\ 202304 Forth2020 Challenge. https://tinyurl.com/LiftChallenge

\ Note0: an 80x25 VT100 compatible terminal is assumed.
\        -Cursor/+Cursor require VT220 support.
\ Note1: _requires_ ANS94 semantics for LEAVE (see 'Show').
\ Note2: environmental dependency: TRUE is assumed to be -1.
\        i.e. _not_ a valid physical floor number (see below).
\ Note3: confirmed working under GNU Forth 0.7.3 and VFX 5.11.
\ Note4: this source code is formatted so as to be "blockable."

\ Glossary of terms:

\ MSC: most significant cell.
\ Physical floor number: a potential stop for the elevator.
\   Valid range is [1 .. #floor].
\ NC: no change.
\ NZB: one based. Floor numbers as entered by the user.
\ TOS: top of the data/parameter stack.
\ Virtual floor number: a number of lines (starting at 0),
\   reflecting the current cabin position upward from the
\   line associated with the user (physical) floor #1.
\ ZB: zero based.

MARKER WasteIt

\ -------------------------------------------------------------
\ Automatically select between Z79Forth/A, GNU Forth or VFX.

: Find79                     \ -- xt|0; Find79 <name>
  BL WORD
  DUP C@ 0= IF DROP ." Missing word name" EXIT THEN
  FIND 0= IF DROP 0 THEN ;

: gf? 1 CELLS 8 = ;
: IFZ7 [ gf?     ]       LITERAL IF POSTPONE \ THEN ;
: IFGF [ gf? 0=  ]       LITERAL IF POSTPONE \ THEN ;
: IFVF [ Find79 upc 0= ] LITERAL IF POSTPONE \ THEN ;

IFZ7 : Off ( addr -- ) 0 SWAP ! ;

\ Following code block borrowed from GNU Forth 0.7.3 vt100.fs.
IFZ7 : pn    BASE @ SWAP DECIMAL 0 U.R BASE ! ;
IFZ7 : ;pn   [CHAR] ; EMIT pn ;
IFZ7 : ESC[  #27 EMIT [CHAR] [ EMIT ;
IFZ7 : AT-XY 1+ SWAP 1+ SWAP ESC[ pn ;pn [CHAR] H EMIT ;

IFGF : UNLESS POSTPONE 0= POSTPONE IF ; IMMEDIATE
IFVF : toupper upc ;

\ -------------------------------------------------------------
\ Variables and constants.

$1B CONSTANT escape          \ ESC key code
8   CONSTANT #floor          \ Number of physical floors
10  CONSTANT #chrmax         \ Max # of chars in cmdbuf
-1  CONSTANT dir_dn          \ Down
0   CONSTANT dir_st          \ Stopped
1   CONSTANT dir_up          \ up
VARIABLE dir_cur             \ Current direction
VARIABLE dir_prv             \ Previous direction
VARIABLE cur_x               \ Cursor column#
VARIABLE cur_y               \ Cursor line#

\ Current "virtual" floor number. 0 -> 1 actual, 2 -> 2 actual,
\ and so on. Bounds are: [0 .. 2*#floor - 2].
VARIABLE vfloor

\ The three flag lists used for scheduling decisions.
CREATE upreqs #floor ALLOT   \ Pending upward requests
CREATE dnreqs #floor ALLOT   \ Pending downward requests
CREATE scheds #floor ALLOT   \ Scheduled stops

CREATE cmdbuf #chrmax ALLOT  \ Input command buffer
VARIABLE #char               \ Queued character count

DEFER ProcessDigit           \ Later set to ProcessGenchar

\ -------------------------------------------------------------
\ Increment the variable at address 'addr'.

: 1+! ( addr -- ) 1 SWAP +! ;

\ -------------------------------------------------------------
\ Turn off the cursor (VT200 control sequence).

: -Cursor ( -- ) escape EMIT ." [?25l" ;

\ -------------------------------------------------------------
\ Turn on the cursor (VT200 control sequence).

: +Cursor ( -- ) escape EMIT ." [?25h" ;

\ -------------------------------------------------------------
\ Return TRUE iff the cabin is currently stopped.

: Stopped? ( -- flag ) dir_cur @ dir_st = ;

\ -------------------------------------------------------------
\ Set cabin status to stopped.

: Stop ( -- flag )
  \ Enforce assertions.
  Stopped?  ABORT" Stopped in 'Stop'"

  dir_cur @ dir_prv !        \ Backup current direction
  dir_st dir_cur ! ;

\ -------------------------------------------------------------
\ Establish the initial state of the system.

: Inits ( -- )
  upreqs #floor ERASE        \ All flags set to FALSE
  dnreqs #floor ERASE
  scheds #floor ERASE

  dir_up dir_cur !           \ Fake initialization
  Stop                       \ Cabin stopped
  #char Off                  \ No characters queued
  vfloor Off                 \ Cabin at floor 1

  0 cur_x !                  \ Initial cursor position
  #floor 1+ 2 * 1+ cur_y ! ; \ in the log area

\ -------------------------------------------------------------
\ Return '#' if ((char *)'flaglist')['offset'] is non-zero,
\ else ' '. XXX

: .Flag ( x y flaglist offset -- )
  + C@ ?DUP IF
    ROT ROT  AT-XY  EMIT  EXIT
  THEN 2DROP ;

\ -------------------------------------------------------------
\ Display pending requests and scheduled stops.

: .FlagInfo ( -- )           \ Y0 line number is 3
  #floor 0 DO                \ I has a ZB physical floor number
    #floor 1- I - 2 * 3 +    \ Y is: (#floor - 1 - I)*2 + Y0

    DUP 4 SWAP upreqs I .Flag \ UP: col 4
    DUP 7 SWAP dnreqs I .Flag \ DN: col 7
       12 SWAP scheds I .Flag \ SCHED: col 12
  LOOP ;

\ -------------------------------------------------------------
\ Return the cabin's current ZB physical floor number.

: PfloorZbCur ( -- zbpfloor_cur ) vfloor @ 2 / ;

\ -------------------------------------------------------------
\ Print the cabin status (DN, -- or UP).
\ Column number (X) is 17.
\ Line number (Y) is 3 + (nfloor - 1) * 2 - vfloor

: .CabinStatus ( -- )
  \ Position the cursor where the cabin is supposed to be.
  17 ( X ) #floor 1- 2 * vfloor @ - 3 + ( Y ) AT-XY

  [CHAR] [ EMIT
  S" DN--UP" DROP dir_cur @ 1+ 2 * + 2 TYPE
  [CHAR] ] EMIT ;

\ -------------------------------------------------------------
\ Display the current status of the system.

: Show ( -- )
  -Cursor  0 0 AT-XY
          ."   LIFT SIMULATOR"
       CR ." #   UP DN SCHED  LIFT"
       CR ." ================+====+==="

  1 #floor DO
    CR  I 2 .R
            ."               |    |"
    CR I 1 = IF LEAVE THEN
          ." ----------------+    +--"
  -1 +LOOP
          ." ================+====+==="

  .FlagInfo
  .CabinStatus

  cur_x @ cur_y @ AT-XY      \ Cursor to log area
  +Cursor ;

\ -------------------------------------------------------------
\ Move the cabin by one vfloor in 'dir_cur @' direction.

: VmoveCabin ( -- )
  \ Enforce assertions.
  Stopped?  ABORT" Stopped in VmoveCabin"

  dir_cur @ vfloor +! ;

\ -------------------------------------------------------------
\ Process valid generic character. In essence, it gets queued
\ to 'cmdbuf' if that buffer is not full. Otherwise the buffer
\ contents gets discarded altogether.

: ProcessGenchar ( char -- char )
  #char @ DUP #chrmax < IF   \ S: char\#char 
    OVER SWAP                \ S: char\char\#char 
    \ Queue the character just entered
    cmdbuf + C!              \ S: char
    #char 1+!                \ Increment #char . S: char
    EXIT
  THEN
  DROP                       \ cmdbuf is full
  #char Off ;                \ Discard buffer contents

\ -------------------------------------------------------------
\ Process valid digit character (in [0-9]).

\ ProcessDigit ( char -- char )
' ProcessGenchar IS ProcessDigit

\ -------------------------------------------------------------
\ Map char (in 'UDG') to the corresponding flaglist.

: char>flaglist ( char\zbpfloor -- char\zbpfloor\flaglist )
  OVER [CHAR] U = IF upreqs EXIT THEN
  OVER [CHAR] D = IF dnreqs EXIT THEN
  OVER [CHAR] G <>  ABORT" Unknown character in char>flaglist"
  scheds ;

\ -------------------------------------------------------------
\ Set flag for 'zbpfloor' in 'flaglist'.
\ Discard inconsistent requests:
\ - out of range physical target floor#.
\ - 'scheds'/'upreqs'/'dnreqs' requests for current 'zbpfloor'
\   and the cabin is stopped.
\ - 'dnreqs' requests from physical floor 1.
\ - 'upreqs' requests from physical floor '#nfloor'.

: UpdateFlaglist ( cmdchar zbpfloor flaglist -- cmdchar )
  \ Check for out of range zbpfloor.
  OVER 0 #floor WITHIN UNLESS 2DROP EXIT THEN

  OVER PfloorZbCur =  Stopped?       AND  IF 2DROP EXIT THEN
  OVER 0=             OVER dnreqs =  AND  IF 2DROP EXIT THEN
  OVER #floor 1- =    OVER upreqs =  AND  IF 2DROP EXIT THEN

  +                          \ S: cmdchar\flagaddr
  OVER [CHAR] G = IF
    [CHAR] M                 \ G triggers a mandatory stop
  ELSE
    OVER                     \ Use the command character
  THEN

  SWAP                       \ S: cmdchar\chrnew\flagaddr.

\ \ Make sure 'M' is not overwritten with 'A'.
\ OVER [CHAR] A = OVER C@ [CHAR] M = AND IF
\   2DROP EXIT
\ THEN

  C! ;                       \ Set flag

\ -------------------------------------------------------------
\ Update the message log area.

: LogMessage ( addr byte-count -- )
  DUP ROT SWAP               \ S: byte-count\addr\byte-count
  TYPE SPACE                 \ Display log message
  ( S: byte-count ) 1+ cur_x +!

  cur_x @ 23 > IF            \ Switch to next line, if needed
    cur_x Off
    cur_y 1+!
  THEN ;

\ -------------------------------------------------------------
\ Process a valid alpha character (in 'UDG'). Such a character
\ must trigger an action in terms of sensor actuation (UP/DN).
\ G type requests schedule a stop right away,

: ProcessAlpha ( char -- char )
  ProcessGenchar             \ Generic input char. processing

  cmdbuf #char @ LogMessage  \ Log the command buffer content

  \ cmdbuf[#char - 1] is in 'UDG'.
  0. cmdbuf #char @ 1- >NUMBER 2DROP
  DROP                       \ Drop convertion result's MSC

  1-                         \ S: char\zbpfloor
  char>flaglist              \ S: char\zbpfloor\flaglist
  UpdateFlaglist             \ S: char
  #char Off ;                \ Discard incoming request

\ -------------------------------------------------------------
\ Return TRUE iff char is in 'UDG'.

: ValidAlpha? ( char -- char flag )
  DUP [CHAR] U = IF TRUE EXIT THEN
  DUP [CHAR] D = IF TRUE EXIT THEN
  DUP [CHAR] G = ;

\ -------------------------------------------------------------
\ Process alpha character if a valid one is recognized.

: ?ProcessAlpha ( char -- char )
  ValidAlpha? IF ProcessAlpha THEN ;

\ -------------------------------------------------------------
\ Return TRUE iff char is in [0-9].

: Digit? ( char -- char flag )
  DUP [CHAR] 0 [CHAR] 9 1+ WITHIN ;

\ -------------------------------------------------------------
\ Process digit character if a valid one is recognized.

: ?ProcessDigit ( char -- char )
  Digit? IF ProcessDigit THEN ;

\ -------------------------------------------------------------
\ Poll the console for valid input characters. Return TRUE
\ only if ESC is recognized, else FALSE.

: ?Buttons ( -- flag )
  BEGIN
    KEY?
  WHILE
    KEY DUP escape = IF DROP TRUE EXIT THEN
    toupper                  \ From GNU Forth (CF resident)
    ?ProcessDigit  ?ProcessAlpha  DROP
  REPEAT FALSE ;

\ -------------------------------------------------------------
\ Return TRUE iff 'vfloor @' does not map to a physical floor.

: BetweenFloors? ( -- flag ) vfloor @ 1 AND ;

\ -------------------------------------------------------------
\ Return TRUE iff there are no scheduled stops.

: SchedStopNone? ( -- flag )
  #floor 0 DO
    I scheds + C@ IF UNLOOP FALSE EXIT THEN
  LOOP TRUE ;

\ -------------------------------------------------------------
\ Return
\ - if 'dir' is > 0: the highest ZB scheduled stop.
\ - if 'dir' is < 0: the lowest ZB scheduled stop.
\ - TRUE if no scheduled stops are found.

: NoSchedStopInDir? ( dir -- TRUE|zbpfloor )
  \ Enforce assertions.
  BetweenFloors? ABORT" Between floors in NoSchedStopInDir?"
  DUP 0=         ABORT" 'dir' is 'dir_st' in NoSchedStopInDir?"

  PfloorZbCur                \ S: dir\zbpfloor_cur
  SWAP 0> IF                 \ dir is UP. S: zbpfloor_cur
    \ Search for the highest request from a floor>TOS.
    1+                       \ Potential loop limit (included)
    DUP #floor = IF DROP TRUE EXIT THEN \ Already at the top

    #floor 1- ( S: zbpfloor_cur+1\#floor-1 ) DO
      I scheds + C@ IF I UNLOOP EXIT THEN
    -1 +LOOP
    TRUE EXIT
  THEN

  \ dir is DN. Search for the lowest request from a floor<TOS.
  0 ( S: zbpfloor_cur\0 ) ?DO
    I scheds + C@ IF I UNLOOP EXIT THEN
  LOOP TRUE ;

\ -------------------------------------------------------------
\ Accept up/dn request. Code shared between 'AcceptUpReq?"
\ and 'AcceptDnReq?'.

: AcceptCmReq ( rqzbpfloor -- TRUE|rqzbpfloor\zbpfloor )
  \ Accept if we're stopped.
  Stopped? IF DROP TRUE EXIT THEN

  \ Accept if no scheduled stops in the current direction.
  dir_cur @ NoSchedStopInDir? DUP TRUE = IF NIP THEN ;

\ -------------------------------------------------------------
\ Accept an UP request iff either of the following is true:
\ - the cabin is stopped.
\ - we're moving up and the request originates from
\   a floor that is on our way.
\ - we're moving down and we have no lower scheduled stop.

: AcceptUpReq? ( rqzbpfloor -- flag )
  \ Deny if there are no pending UP requests from rqzbpfloor.
  DUP upreqs + C@ UNLESS DROP FALSE EXIT THEN

  \ Common acceptance criteria.
  AcceptCmReq DUP TRUE = IF EXIT THEN

  dir_cur @ 0> IF            \ Going up
    \ S: rqzbpfloor\highest_sched_zbpfloor
    1+  PfloorZbCur  SWAP  WITHIN
    EXIT
  THEN

  \ Going down. S: rqzbpfloor\lowest_sched_zbpfloor
  < ;

\ -------------------------------------------------------------
\ Accept a DN request iff either of the following is true:
\ - the cabin is stopped.
\ - we're moving down and the request originates from
\   a floor that is on our way.
\ - we're moving up and we have no higher scheduled stop.

: AcceptDnReq? ( rqzbpfloor -- flag )
  \ Deny if there are no pending DN requests from rqzbpfloor.
  DUP dnreqs + C@ UNLESS DROP FALSE EXIT THEN

  \ Common acceptance criteria.
  AcceptCmReq DUP TRUE = IF EXIT THEN

  dir_cur @ 0< IF            \ Going down
    \ S: rqzbpfloor\lowest_sched_zbpfloor
    PfloorZbCur 1+ WITHIN
    EXIT
  THEN

  \ Going up. S: rqzbpfloor\highest_sched_zbpfloor
  > ;

\ -------------------------------------------------------------
\ 'Compute' is the sensor update routine. It checks
\ unacknowleded UP and DN requests and transforms them into
\ scheduled stops, when/if appropriate. The pending requests
\ are not cleared until the cabin reaches a floor from which it
\ was called.

\ Unacknowledged requests might originate from flags set in
\ the 'upreqs' on 'dnreqs' byte arrays. Those flags are updated
\ by the 'ProcessAlpha' primitive.

\ 'Compute' does not decide what to do next--that
\ responsibility is deleguated to the 'Step' primitive.

: Compute ( -- )
  BetweenFloors? IF EXIT THEN  \ NC if between floors

  \ Update the 'scheds' flag list, if appropriate.
  #floor 0 DO
    I AcceptUpReq? IF        \ Accept UP req. from floor I
      [CHAR] A I scheds UpdateFlaglist DROP
    THEN

    I AcceptDnReq? IF        \ Accept DN req. from floor I
      [CHAR] A I scheds UpdateFlaglist DROP
    THEN
  LOOP ;

\ -------------------------------------------------------------
\ Acknowledge only the request for 'dir_cur @' if a matching
\ flag is set for 'reqzbpfloor' otherwise for the opposite
\ direction. The need to consider the opposite direction
\ arises when the cabin must change direction.

: AckRequestsInCurrentDir ( reqzbpfloor -- )
  \ Enforce assertions.
  Stopped?  ABORT" Stopped in AckRequestsInCurrentDir"

  dnreqs upreqs
  dir_cur @ 0< IF SWAP THEN    \ Going down. Swap pointers.

  ( S: reqzbpfloor\odirfl\cdirfl)
  DUP 3 PICK + C@ IF NIP ELSE DROP THEN
  + FALSE SWAP C! ;            \ Clear flag

\ -------------------------------------------------------------
\ Bypass acknowledgement (and return TRUE) if either:
\ - moving up and pending DN and no UP request and a stop
\   is scheduled for an upper floor.
\ - moving down and pending UP and no DN request and a stop
\   is scheduled for a lower floor.
\ Otherwise return FALSE.

: BypassStopAtCurFloor? ( -- flag )
  dnreqs upreqs
  dir_cur @ 0< IF SWAP THEN  \ Going down. Swap pointers.

  PfloorZbCur + C@ 0=  SWAP PfloorZbCur + C@ AND IF
    dir_cur @ NoSchedStopInDir? TRUE <> IF TRUE EXIT THEN
  THEN

  FALSE ;

\ -------------------------------------------------------------
\ Acknowledge a scheduled stop for the current physical floor.
\ Return TRUE if there was one, else FALSE.

: AckScheds? ( -- flag )
  \ Enforce assertions.
  BetweenFloors?  ABORT" Between floors in AckScheds?"
  Stopped?        ABORT" Stopped in AckScheds?"

  PfloorZbCur scheds + DUP C@ IF
    BypassStopAtCurFloor? UNLESS
      FALSE SWAP C!  PfloorZbCur AckRequestsInCurrentDir
      TRUE EXIT              \ Notify intent to stop
    THEN
  THEN
  DROP FALSE ;               \ Don't stop me now!

\ -------------------------------------------------------------
\ Power, force, motion, drive. Propaganda!
\ We are de facto implementing a state machine here.

: Step ( -- )
  \ If we're between floors, just move the cabin UP/DN a line.
  BetweenFloors? IF VmoveCabin EXIT THEN

  \ We are on a physical level.
  \ If the cabin is stopped, we need to select a direction,
  \ based on scheduled stops, with a preference for dir_prv.
  Stopped? IF
    \ If there are no scheduled stops, we stay stopped.
    SchedStopNone? IF EXIT THEN

    dir_prv @ DUP NoSchedStopInDir? TRUE = IF
      NEGATE                 \ -dir_prv (opposite direction)
    THEN
    dir_cur !                \ Select direction
    VmoveCabin EXIT
  THEN

  \ The cabin is not stopped. Check for scheduled stops for
  \ that floor. If there are any, acknowledge them and stop.
  AckScheds? IF Stop EXIT THEN

  \ No scheduled stop for that floor, proceed normally.
  VmoveCabin ;

\ -------------------------------------------------------------
: Run ( -- )
  Inits PAGE
  BEGIN
    Show  2000 MS
    ?Buttons                 \ S: <exit requested flag>
    Compute  Step
  UNTIL ;

\ Entry point here.
Run  WasteIt


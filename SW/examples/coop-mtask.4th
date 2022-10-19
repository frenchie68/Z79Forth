\ See https://forth-ev.de/wiki/res/lib/exe/fetch.php/
\ vd-archiv:4d2021-01.pdf for the original code.
\
\ Z79Forth adaptations, Francois Laagel, May 21st 2022.
\ This package implements co-routines. Re-scheduling takes
\ place when PAUSE is invoked. Matthias's implementation is
\ canonical in that is assumes that return addresses are stored
\ in the return stack, which is not the case with Z79Forth.
\ Actually return addresses are stored in the system stack.
\ They can be retrieved by resorting to "S @" and altered
\ with "<addr> S !" However, at some point, the return address
\ of the interpreter's main loop will be lost. So the only
\ way to stop an application based on this API will be through
\ some sort of interrupt (RESET or Control-C from the console).
\ As a consequence of this, we require an extra field in the
\ task descriptor data structure (similar to the Unix U-area)
\ in order to keep track of the code return address.
\ To keep the code changes to a minimum, that field has
\ been added to the very end of the task status structure,
\ i.e. at offset (4+stkspccells*2) cells. That address is
\ returned by the task-cra primitive.
\
\ Limitations: Z79Forth does _not_ implement user variables
\ and has them part of a global context instead. Care should
\ be taken so that they are not altered between task switches.
\ In practice, this means that BASE should never be changed
\ between context switches. This also applies to SCR, BLK, >IN
\ and friends. Calls to PAUSE (aka POSIX pthread_yield) should
\ be extremely carefully placed in the application code.
\ Things like PAD and words that depend on it also should be
\ handled between clearly identified re-scheduling points.
\
\ An attempt at documenting the API:
\ TASK: <task-decriptor-name> creates a blank task descriptor.
\ No entry point is assigned at that point.
\
\ Application configuration consists in declaring all tasks
\ and their associated entry points. This is done by resorting
\ either to ACTIVATE or BACKGROUND. Either primitive must be
\ called before PAUSE is.
\
\ A currently running task may elect to suspend itself by
\ invoking STOP--please note that STOP itself calls PAUSE!
\ It may also choose to resume a suspended arbitrary task by
\ calling WAKE or to suspend an arbitrary task by calling IDLE.
\
\ Re-scheduling points occurr when PAUSE is called. Prior to
\ that either MULTITASK or SINGLETASK must be called. This
\ defines the behaviour of PAUSE.
\
\ -----------------------------------------------------------

\ Z79Forth glue code begins.
: rdrop R> DROP ;
1 CONSTANT true
0 CONSTANT false
: rdepth $12E C@ ;               \ Release dependent!!!
: nop ;
: cell- ( addr1 -- addr2 ) 1 CELLS - ;
: DEFER ( "name" -- )
  CREATE ['] ABORT ,
  DOES> ( ... -- ... )
    @ EXECUTE ;
: DEFER! ( xt1 xt2 -- ) 9 + ! ;  \ Z79Forth impl. dependent
: IS ( xt "<spaces>name" -- ) STATE @ IF
    POSTPONE [']   POSTPONE DEFER!
  ELSE
    ' DEFER!
  THEN
; IMMEDIATE
DEFER PAUSE
\ Z79Forth glue code ends.

\ Configuration:

32 CELLS CONSTANT stackspace
stackspace 1 CELLS / CONSTANT stkspccells

\ Internal stucture of task memory (c stands for CELLS):
\ 0: Pointer to next task
\ +1c: Task currently active ?
\ +2c: Parameter stack depth
\      +3c - +(3+stkspccells-1)c: Parameter stack
\ +(3+stkspccells)c: Return stack depth
\      (4+stkspccells)c - (4+stkspccells*2-1)c: Return stack
\ +(4+stkspccells*2)c: co-routine code return address
\ (5+stkspccells*2)c: Complete size of data structure.

VARIABLE up           \ U-area
0 up !                \ Initialize variable
\ Current task fields accessors
: next-task ( -- task ) up @ ;
: task-state ( -- state ) up @ cell+ ;
: task-data ( -- data ) up @ 2 CELLS + ;
: task-return ( -- return ) up @ 3 CELLS + stackspace + ;
: task-cra ( -- cra ) up @ 4 stkspccells 2 * + CELLS + ;

\ -----------------------------------------------------------
: (pause) ( stacks fly around )

  S cell+ @ task-cra !       \ Save code return address

\ Parameter stack backup.
  depth task-data !          \ Number of elements
  task-data cell+ >R         \ Begin with top of stack

  BEGIN
    DEPTH
  WHILE
    R@ !
    R> cell+ >R
  REPEAT

  rdrop

\ -----------------------------------------------------------
\ Return stack backup.

  rdepth task-return !       \ Number of elements
  task-return cell+          \ Begin with top of return stack

  BEGIN
    rdepth
  WHILE
    R> OVER !
    cell+
  REPEAT

  DROP

\ -----------------------------------------------------------
\ Task scheduling algorithm. We might loop here forever
\ if no task is runnable.

  BEGIN
    next-task @ up !         \ Switch to next running task
    task-state @             \ Runnable?
  UNTIL

\ -----------------------------------------------------------
\ Return stack restore.

  task-return @              \ Return stack depth
  CELLS task-return +        \ Begin with end of stack

  BEGIN
    DUP task-return <>
  WHILE
    DUP @ >R
    cell-
  REPEAT

  DROP

\ -----------------------------------------------------------
\ Paramater stack restore.

  task-data @                \ Data stack depth
  CELLS task-data +          \ Begin with end of return stack
  >R

  BEGIN
    R@ task-data <>
  WHILE
    R@ @
    R> cell- >R
  REPEAT

  rdrop

  task-cra @ S cell+ !       \ Context switch
;

\ -----------------------------------------------------------

\ Wake a random task (IRQ safe)
: WAKE ( task -- ) cell+ true SWAP ! ;

\ Idle a random task (IRQ safe)
: IDLE ( task -- ) cell+ false SWAP ! ;

\ -----------------------------------------------------------
\ Round-robin list task handling - do not use in IRQ !
\ -----------------------------------------------------------

\ Stop current task
: STOP ( -- ) false task-state ! PAUSE ;

\ Generate jump opcodes
: MULTITASK ( -- ) ['] (pause) IS PAUSE ;

\ to be stored in pause
: SINGLETASK ( -- ) ['] nop IS PAUSE ;

\ Checks if a task is currently inside of round-robin list
\ (do not use in IRQ)
: task-in-list? ( task -- flag )
  next-task
  BEGIN
    ( Task-Address )
    2DUP = IF 2DROP true EXIT THEN
    @ DUP next-task = \ Stop when end of circ. list is reached
  UNTIL
  2DROP false
;

: previous ( task -- addr-of-task-before )
  \ Find the task that has the desired one in its next field
  >R next-task
  BEGIN
    DUP @ R@ <>
  WHILE
    @
  REPEAT
  rdrop
;

: insert ( task -- ) \ Insert a task into the round-robin list
  DUP task-in-list?  \ Is the desired task currently linked?
  IF
    DROP
  ELSE
    next-task @ OVER ! next-task !
  THEN
;

: remove ( task -- ) \ Remove a task from the round-robin list
  DUP task-in-list?  \ Is the desired task currently linked?
  IF
    DUP @ ( task next )
    SWAP previous ( next previous ) !
  ELSE
    DROP
  THEN
;

\ -----------------------------------------------------------
\ Create a new task - do not use in IRQ !
\ In Z79Forth no dictionary words should ever be called from
\ interrupt context.
\ -----------------------------------------------------------
: task: ( "name" -- ) CREATE stackspace 2 * 5 CELLS + ALLOT ;

: preparetask ( task-entryp task -- )
  >R ( R: task )

  \ First task declared initializes the 'up' variable
  \ and the task in the descriptor at offset zero points
  \ to itself.
  up @ 0= IF R@ DUP ! R@ up ! THEN

  0 R@ 2 CELLS + !              \ Empty data stack
  0 R@ 3 CELLS + stackspace + ! \ Empty the return stack

  \ Store the desired entry point in the task descriptor
  R@ 4 CELLS + stackspace 2 * + !

  R> insert
;

: ACTIVATE ( task-entryp task -- )
  true OVER cell+ !  \ Currently running
  preparetask
;

: BACKGROUND ( task-entryp task -- )
  false OVER cell+ ! \ Currently idling
  preparetask
;

\ -----------------------------------------------------------
\ Trial run (purely accademic)

TASK: background-task
: background-code
  BEGIN
    [CHAR] . EMIT CR
    STOP
  AGAIN
;

TASK: main-task
: main-code CR
  BEGIN
    5 0 DO
      I 1+ . 1000 MS
      KEY? IF KEY ABORT THEN
    LOOP
    background-task WAKE
    PAUSE
\   S H. CR     \ Optional sanity check
  AGAIN
;

' main-code main-task ACTIVATE
' background-code background-task BACKGROUND

MULTITASK   \ Select the scheduling algorithm
main-code   \ Initiate the main task


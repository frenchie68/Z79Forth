\ Hexadoku Solver. Francois Laagel.                May 11, 2023

\ The interesting thing about this algorithm is that it does
\ not work by looking for a solution. It works by systematic
\ refutation of possibilities leading to constraint violations.
\ Eventually, a solution might emerge--or not. There could be
\ several solutions to a weakly defined problem.
\
\ This code can be operated as either a problem solver or as
\ a potential puzzle validator (solutions enumerator/counter).
\ The 'stopon1st' user tunable parameter selects which
\ behaviour is to be achieved. If TRUE, the program will act
\ as a solver, assuming that only one solution exists and
\ display the solving progress incrementally. Otherwise, it
\ will act as a puzzle verifier and only display the total
\ number of solutions it was able to come across. Performance
\ and statistical data will only be shown in the solver mode.
\
\ This code targets GNU Forth 0.7.3 (8 byte cell). It can also
\ be run SwiftForth 3.12.0 (4 byte cell), VFX Forth 64
\ 5.41 or Z79Forth/A.
\
\ "There's no excuse for not writing programs that are
\ portable." Captain Grace Hopper, 1982.
\ https://www.youtube.com/watch?v=_bP14OzIJWI

\ -------------------------------------------------------------
\ Glossary:
\
\ grid: a 16 row by 16 columns structure consisting of spots.
\
\ spot: an unsigned 16 bit integer (stored as a cell) that
\ represents either:
\ - a resolved term for the hexadecimal number residing at
\   the corresponding location. A resolved term is an exact
\   power of two, i.e. only one bit is set for that spot.
\ - or the logical sum of possible values for that spot.
\   This would be the sum of all possibilities, in quantum
\   theory terms. Such a number has more than one bit set.
\
\ fixed point: a grid cell that is resolved, i.e. has a
\ provisional value that is a power of two (non-zero). Such a
\ cell value cannot be altered by mask application/filtering.
\
\ transaction: a set of saved states made immediately prior
\ to an original speculative decision and covering all
\ subsequently inferred changes before a dead end situation is
\ detected or a nested speculative decision is made. This is
\ the basis for an undo log buffer (aka transaction stack).
\
\ A spot having zero for its value indicates a dead end in the
\ current problem resolution state. This program strives to
\ behave so as to avoid that from ever happening.

\ -------------------------------------------------------------
DECIMAL
MARKER wasteit

\ Following code block borrowed from GNU Forth 0.7.3 vt100.fs.
: pn    BASE @ SWAP DECIMAL 0 U.R BASE ! ;
: ;pn   [CHAR] ; EMIT pn ;
: esc[  #27 EMIT [CHAR] [ EMIT ;
: AT-XY 1+ SWAP 1+ SWAP esc[ pn ;pn [CHAR] H EMIT ;

: log2 -1 BEGIN
    OVER 
  WHILE
    1+ SWAP 1 RSHIFT SWAP
  REPEAT NIP ;

\ The colon-sys aberration in the 1994 standard prevents us
\ from being smart. colon-sys may or not be of size zero and
\ the control flow stack may or may not be the data stack.
1 CELLS log2 CONSTANT cellrshft
: cell/   [ cellrshft    ] LITERAL RSHIFT ;
: 2cells/ [ cellrshft 1+ ] LITERAL RSHIFT ;

: 2nip 2SWAP 2DROP ;           \ VfX requires this as well

: 16* 4 LSHIFT ;
: 16/mod DUP $F AND SWAP 4 RSHIFT ;
: 1+! 1 SWAP +! ;
: 1-! -1 SWAP +! ;

\ -------------------------------------------------------------
\ Variables and constants.

FALSE CONSTANT stopon1st       \ User tunable. No vis. if FALSE
FALSE VALUE logtrans   \ If NZ, log changes to the trans. stack
BL CONSTANT wildc
VARIABLE unknowns
VARIABLE solutions

CREATE grid 256 CELLS ALLOT    \ 16x16 is the problem size

\ A transaction is the unit of rollbacks (undos). It is defined
\ as the set of grid saved states between the time we make a
\ speculative choice and the time when a constraint violation
\ is detected or when a nested speculative choice is made
\ (excluded).

4096 CONSTANT tstk-nitems
CREATE tstack tstk-nitems 2* CELLS ALLOT
HERE CONSTANT tstk-bottom
\ Each entry on the transaction stack is:
\ TOS:  saved-bitmask           1 CELL
\ TOS+1: mxxx.xxxx:yyyy.yyyy    1 CELL
\       bit #15:        beginning of transaction marker.
\       bits #14-8:     xcol: 0..15
\       bit #7-0:       yrow: 0..15
VARIABLE tstkp

\ Statistical data support.

VARIABLE reclev                \ Current recursion level
VARIABLE reclevmax             \ Maximum recursion level
VARIABLE nbt                   \ # of backtracks
CREATE ncb 2 CELLS ALLOT       \ # of calls to countbits double

: d1+! DUP 2@ 1. D+ ROT 2! ;

\ -------------------------------------------------------------
\ Bit count utilities.

\ Adapted from "Hacker's Delight" Second Edition
\ by Henry S. Warren Jr., Edt by Addison-Wesley
\ Chapter 5 "Counting bits", page 82.

: countbits ( uu -- #bits )
  ncb d1+!
  DUP 1 RSHIFT $5555 AND -
  DUP $3333 AND SWAP 2 RSHIFT $3333 AND +
  DUP 4 RSHIFT + $0F0F AND
  DUP 8 RSHIFT +
  $1F AND ;

\ Compute 2^n fast, i.e. faster than LSHIFT can do it.
\ Note: 'n' is restricted to the [0..15] range.
CREATE exptbl
1     , 2     , 4     , 8     ,
$10   , $20   , $40   , $80   ,
$100  , $200  , $400  , $800  ,
$1000 , $2000 , $4000 , $8000 ,

: 2^n ( n -- 2^n ) CELLS exptbl + @ ;

\ Contributed by Bob Armstrong.
: pow2? ( n -- f )
  DUP DUP 1- AND 0= SWAP 0<> AND ;

\ -------------------------------------------------------------
\ Incremental grid visualization.

: getxy-from-grid-addr ( saddr - x y )
  grid - cell/ 16/mod ;

: |visual ( val saddr -- val saddr )
  \ No visualization if looking for multiple solutions.
  stopon1st 0= IF EXIT THEN

  OVER pow2? IF                \ Spot value is known
    OVER 16 0 DO
      DUP I 2^n = IF
        DROP I
        DUP 10 < IF [CHAR] 0 ELSE [CHAR] 7 THEN
        + LEAVE
      THEN
    LOOP
  ELSE
    wildc
  THEN

  \ S: val\saddr\char-from-val
  \ Return immediately if char==wildc and bitcount(saddr@)<>1
  \ This corresponds to a situation where a given cell's mask
  \ changes but the spot remains unresolved.
  OVER @ pow2? 0= OVER wildc = AND IF
    DROP EXIT
  THEN

  OVER getxy-from-grid-addr    \ S: val\saddr\char-from-val\x\y
  SWAP 2* SWAP AT-XY EMIT ;

\ -------------------------------------------------------------
\ Transaction stack handling (undo log).

\ The following does not exist in GForth/0.7.3 or Z79Forth/A.
: cell- 1 CELLS - ;

: tstk-push ( begin-flag ptr -- )
  \ We need exactly two cells. Is enough room available?
  tstkp @ tstack - 2cells/ 0=
    ABORT" Transaction stack overflow"

  \ Extract x and y from the 'ptr' pointer.
  DUP >R
  getxy-from-grid-addr         \ R: ptr, S: begin-flag\x\y
  SWAP ROT                     \ R: ptr, S: y\x\begin-flag
  IF $80 OR THEN
  8 LSHIFT OR

       tstkp @ cell- DUP tstkp ! !
  R> @ tstkp @ cell- DUP tstkp ! ! ;

: tstk-pop ( -- begin-flag )
  \ At least two cells need to be stacked up.
  tstk-bottom tstkp @ - 2cells/ 0=
    ABORT" Transaction stack underflow"

  tstkp @ DUP @ >R             \ R: bitmask, S: tsktp@
  CELL+ tstkp !

  tstkp @ DUP @ >R             \ R: bitmask\mX:Y, S: tsktp@
  CELL+ tstkp !

  R> DUP $8000 AND             \ R: bitmask, S: mX:Y\begin-flg
  SWAP $7FFF AND               \ R: bitmask, S: begin-flg\X\Y

  DUP 8 RSHIFT SWAP $FF AND    \ R: bitmask, S: begin-flg\X\Y
  16* + CELLS grid +           \ R: bitmask, S: begin-flg\saddr
  R> SWAP                      \ S: begin-flg\bitmask\saddr

  \ Check whether we are going from resolved to unresolved.
  \ If so increment 'unknowns' accordingly.
  DUP @ pow2? IF               \ S: begin-flg\bitmask\saddr
    \ XXX: this assumes 'bitmsk' is NZ!!!
    OVER pow2? 0= IF
      unknowns 1+!
    THEN
  THEN

  |visual ! ;                  \ Implicit update-spot

\ -------------------------------------------------------------
\ Initializations.

: char>digit ( char -- char|digitval )
  DUP [CHAR] 0 [CHAR] 9 1+ WITHIN IF [CHAR] 0 - EXIT THEN
  DUP [CHAR] A [CHAR] F 1+ WITHIN IF [CHAR] 7 - THEN ;

: initline ( srcaddr bytecount linenum -- )
  16* CELLS grid +             \ S: srcaddr\bytecount\tgtaddr
  SWAP 0 DO                    \ S: srcaddr\tgtaddr
    OVER I + C@ char>digit DUP 16 < IF
      2^n                      \ S: srcaddr\tgtaddr\2^<digit>
      OVER !                   \ S: srcaddr\tgtaddr
      CELL+                    \ S: srcaddr\tgtaddr-next
      unknowns 1-!
    ELSE
      [CHAR] : <> IF CELL+ THEN
    THEN
  LOOP
  2DROP ;

: inits ( -- )
  0 solutions !
  256 unknowns !
  grid 256 0 DO
    $FFFF OVER !
    CELL+
  LOOP
  DROP

  \ Empty grid.
\ S" ....:....:....:...." 0  initline
\ S" ....:....:....:...." 1  initline
\ S" ....:....:....:...." 2  initline
\ S" ....:....:....:...." 3  initline

\ S" ....:....:....:...." 4  initline
\ S" ....:....:....:...." 5  initline
\ S" ....:....:....:...." 6  initline
\ S" ....:....:....:...." 7  initline

\ S" ....:....:....:...." 8  initline
\ S" ....:....:....:...." 9  initline
\ S" ....:....:....:...." 10 initline
\ S" ....:....:....:...." 11 initline

\ S" ....:....:....:...." 12 initline
\ S" ....:....:....:...." 13 initline
\ S" ....:....:....:...." 14 initline
\ S" ....:....:....:...." 15 initline

  \ Original design (1.2+M backtracks).
  S" 0...:.5.7:.9.B:.DEF" 0  initline
  S" 45..:C..F:...2:..AB" 1  initline
  S" ..A.:..2.:.D..:.5.7" 2  initline
  S" C..F:8.A.:.5..:01.3" 3  initline

  S" 1.0.:7..4:D...:..C6" 4  initline
  S" .A..:.3.5:..4.:...9" 5  initline
  S" 6..8:..1.:72..:5..4" 6  initline
  S" ..9.:2..C:E3.8:..1." 7  initline

  S" 2..1:5..6:...C:...." 8  initline
  S" B..C:..D.:.8..:.F.5" 9  initline
  S" ....:.B.1:..0.:6..A" 10 initline
  S" 5...:..F.:...D:..0." 11 initline

  S" 3..0:B..A:.6.1:E..8" 12 initline
  S" 9.C.:..7.:30..:.A.1" 13 initline
  S" A.D.:..3.:.E..:.6.0" 14 initline
  S" .8..:E0..:..C.:D4.." 15 initline

  \ Transaction stack initialization.
  tstk-bottom tstkp !
  FALSE TO logtrans

  \ Statistical data initialization.
  0. ncb 2!                    \ Number of calls to countbits
  0 nbt !                      \ Number of backtracks
  0. reclev ! reclevmax ! ;    \ Recursion level inits

\ -------------------------------------------------------------
\ Visualization.

\ Underline character rendition on.
: +ul ( -- )
  stopon1st 0= IF EXIT THEN
  #27 EMIT ." [4m" ;

\ Underline character rendition off.
: -ul ( -- )
  stopon1st 0= IF EXIT THEN
  #27 EMIT ." [m" ;

\ Turn off the cursor (VT200 control sequence).
: -cursor ( -- )
  stopon1st 0= IF EXIT THEN
  #27 EMIT ." [?25l" ;

\ Turn on the cursor (VT200 control sequence).
: +cursor ( -- )
  stopon1st 0= IF EXIT THEN
  #27 EMIT ." [?25h" ;

: mask>char ( mask -- char )
  DUP pow2? IF                 \ S: mask\nbits
    16 0 DO
      DUP I 2^n = IF
        DROP I UNLOOP
        DUP 10 < IF [CHAR] 0 ELSE [CHAR] 7 THEN
        + EXIT
      THEN
    LOOP
    1 ABORT" WTF?"             \ This should never be executed
  THEN
  DROP wildc ;

: display-grid ( -- )
  grid 16 0 DO                 \ J has the current row#
    16 0 DO                    \ I has the current col#
      DUP @ mask>char
        EMIT SPACE
      CELL+
    LOOP
    CR
  LOOP DROP ;

\ -------------------------------------------------------------
\ Primary way of altering a grid's spot but not the only one!
\ 'speculate' does unregistered grid changes as well.
\ The only changes we can see here are inferred. Undo
\ operations are carried out via 'tstk-pop'.

: update-spot ( val saddr -- )
  2DUP @ = IF                  \ Value not changed
    2DROP EXIT
  THEN

  \ This update resolves the spot pointed to by 'saddr'.
  OVER pow2? IF unknowns 1-! THEN

  logtrans IF                  \ Transaction is logged
    FALSE OVER tstk-push
  THEN
  |visual ! ;

\ -------------------------------------------------------------
\ Four by four exclusion/filtering.

\ No side effects.
: getmask4 ( xcol yrow -- mask\FALSE | TRUE )
  0                            \ Sanity check
  $FFFF                        \ Initial mask
  \ S: xcol\yrow\check\mask
  4 0 DO                       \ J has dy
    4 0 DO                     \ I has dx
      3 PICK I +               \ Absolute col#
      3 PICK J + 16* +
      CELLS grid +
      @ DUP pow2? IF
        \ S: xcol\yrow\check\mask\val
        ROT OVER  \ S: xcol\yrow\mask\val\check\val
        2DUP AND  \ S: xcol\yrow\mask\val\check\val\(check&val)

        IF                     \ Bit already set!!!
          2DROP 2DROP 2DROP UNLOOP UNLOOP TRUE EXIT
        THEN

        \ S: xcol\yrow\mask\val\check\val
        OR               \ S: xcol\yrow\mask\val\(check|val)
        -rot             \ S: xcol\yrow\(check|val)\mask\val
        INVERT AND       \ S: xcol\yrow\(check|val)\(mask&~val)
      ELSE
        DROP
      THEN
    LOOP
  LOOP
  \ S: xcol\yrow\check\mask
  NIP NIP NIP FALSE ;

: setmask4 ( xcol yrow mask -- failure-flag )
  \ If 'mask' is zero, it means that all cells in that 4x4
  \ quadrant are resolved (fixed points). Just return a success
  \ indication, should such a condition occur.
  ?DUP 0= IF 2DROP FALSE EXIT THEN

  -rot                         \ S: mask\xcol\yrow
  4 0 DO                       \ J has dy
    4 0 DO                     \ I has dx
      OVER I +                 \ Absolute col#
      OVER J + 16* +
      CELLS grid +             \ S: mask\xcol\yrow\saddr
      DUP @ DUP pow2? IF
        2DROP
      ELSE
        \ S: mask\xcol\yrow\saddr\sval
        4 PICK AND           \ S: mask\xcol\yrow\saddr\sval-new
        ?DUP IF
          SWAP update-spot
        ELSE \ Mask application would result in zero spot value
          2DROP 2DROP UNLOOP UNLOOP TRUE EXIT
        THEN
      THEN
      \ S: mask\xcol\yrow
    LOOP
  LOOP
  DROP 2DROP FALSE ;

\ 4x4 block logic: either a spot is known or the list
\ of alternatives must exclude all known spots values.
: reduce4x4 ( -- failure-flag )
  4 0 DO                       \ Iterate over quadrant rows
    4 0 DO                     \ Iterate over quadrant columns
      I 4 * J 4 *
      2DUP getmask4 IF
        2DROP UNLOOP UNLOOP TRUE EXIT
      THEN
      ( S: xcol#\yrow#\new-possibly-zero-mask ) setmask4 IF
        UNLOOP UNLOOP TRUE EXIT
      THEN
    LOOP
  LOOP
  FALSE ;

\ -------------------------------------------------------------
\ Horizontal exclusion/filtering.

\ ANS94 3.2.3.3 Return stack:
\ A program shall not access from within a DO-LOOP values
\ placed on the return stack before the loop was entered.
\ Note: this is enforced in SwiftForth but not in GForth.

\ No side effects.
: get-horiz-mask ( yrow -- mask\FALSE | TRUE )
  16* CELLS grid +             \ Start of row address
  0                            \ Sanity check
  $FFFF                        \ Initial mask
  16 0 DO                      \ Iterate over columns
    \ srow-addr\check\mask
    2 PICK I CELLS + @ DUP pow2? IF
      \ srow-addr\check\mask\val
      ROT OVER \ srow-addr\mask\val\check\val
      2DUP AND \ srow-addr\mask\val\check\val\(check&val)

      IF                       \ Bit is already set!!!
        UNLOOP 2DROP 2DROP DROP TRUE EXIT
      THEN

      \ srow-addr\mask\val\check\val
      OR                       \ srow-addr\mask\val\(check|val)
      -rot                     \ srow-addr\(check|val)\mask\val
      INVERT AND
    ELSE
      DROP
    THEN
  LOOP
  \ srow-addr\check\mask
  NIP NIP FALSE ;

: set-horiz-mask ( yrow mask -- failure-flag )
  \ If 'mask' is zero. just return a success indication.
  ?DUP 0= IF DROP FALSE EXIT THEN

  SWAP
  16* CELLS grid +
  16 0 DO                      \ Iterate over columns
    DUP @ DUP pow2? IF
      DROP
    ELSE
      \ S: mask\saddr\sval
      2 PICK AND               \ S: mask\saddr\sval-new
      ?DUP IF
        OVER update-spot
      ELSE \ Mask application would result in zero spot value
        2DROP UNLOOP TRUE EXIT
      THEN
    THEN
    \ S: mask\saddr
    CELL+
  LOOP
  2DROP FALSE ;

\ -------------------------------------------------------------
\ Vertical exclusion/filtering.

\ No side effects.
: get-vert-mask ( xcol -- mask\FALSE | TRUE )
  CELLS grid +                 \ Start of column address
  0                            \ Sanity check
  $FFFF                        \ Initial mask
  16 0 DO                      \ Iterate over rows
    \ scol-addr\check\mask
    2 PICK I 16* CELLS + @ DUP pow2? IF
      \ scol-addr\check\mask\val
      ROT OVER \ scol-addr\mask\val\check\val
      2DUP AND \ scol-addr\mask\val\check\val\(check&val)

      IF                       \ Bit is already set!!!
        UNLOOP 2DROP 2DROP DROP TRUE EXIT
      THEN

      \ scol-addr\mask\val\check\val
      OR                       \ scol-addr\mask\val\(check|val)
      -rot                     \ scol-addr\(check|val)\mask\val
      INVERT AND
    ELSE
      DROP
    THEN
  LOOP
  \ srow-addr\check\mask
  NIP NIP FALSE ;

: set-vert-mask ( xcol mask -- failure-flag )
  \ If 'mask' is zero. just return a success indication.
  ?DUP 0= IF DROP FALSE EXIT THEN

  SWAP
  CELLS grid +                 \ Beginning of column address
  16 0 DO                      \ Iterate over rows
    DUP @ DUP pow2? IF
      DROP
    ELSE
      \ S: mask\saddr\sval
      2 PICK AND               \ S: mask\saddr\sval-new
      ?DUP IF
        OVER update-spot       \ S: mask\saddr
      ELSE \ Mask application would result in zero spot value
        2DROP UNLOOP TRUE EXIT
      THEN
    THEN
    \ S: mask\saddr
    16 CELLS +
  LOOP
  2DROP FALSE ;

: reduceall ( -- failure-flag )
  reduce4x4 IF                 \ Constraint violated
    TRUE EXIT
  THEN

  16 0 DO
    I get-horiz-mask IF        \ Constraint violated
      UNLOOP TRUE EXIT
    THEN
    ( S: new-possibly-zero-mask ) I SWAP set-horiz-mask IF
      UNLOOP TRUE EXIT
    THEN

    I get-vert-mask IF         \ Constraint violated
      UNLOOP TRUE EXIT
    THEN
    ( S: new-possibly-zero-mask ) I SWAP set-vert-mask IF
      UNLOOP TRUE EXIT
    THEN

  LOOP
  FALSE ;

\ -------------------------------------------------------------
\ Speculation.

: infer ( -- success-flag )
  256                          \ 'unknowns' worst case scenario
  BEGIN
    reduceall IF DROP FALSE EXIT THEN
    unknowns @ >
  WHILE
    unknowns @
  REPEAT TRUE ;

\ "[Gordon Gekko]: The point is ladies and gentlemen that
\ greed, for lack of a better word, is good." from the "Wall
\ Street" movie by Oliver Stone, 1987.
: get-unresolved ( -- grid-cell-addr | FALSE )
  grid DUP @ countbits          \ minp\minp@#bits
  OVER CELL+                    \ minp\minp@#bits\curp

  255 0 DO
    DUP @ countbits           \ minp\minp@#bits\curp\curp@#bits

    \ The newly selected minimum bit count cannot ever be 1.
    DUP 1 = IF
      DROP
    ELSE
      \ minp@#bits can be 1, indicating a resolved spot.
      \ If it is, accept anything but 1 as a new minimum.
      2 PICK 1 = IF           \ minp\minp@#bits\curp\curp@#bits
        2nip 2DUP
      THEN

      \ minp\minp@#bits\curp\curp@#bits
      DUP 2 = IF               \ 2 as a good enough minimum
        2nip OVER LEAVE        \ curp\curp@#bits\curp
      THEN

      DUP 3 PICK < IF         \ minp\minp@#bits\curp\curp@#bits
        2nip OVER              \ curp\curp@#bits\curp
      ELSE
        DROP
      THEN
    THEN

    CELL+                      \ minp\minp@#bits\curp
  LOOP
  DROP                         \ minp\minp@#bits

  \ If the minimum bit count is 1 the problem is solved.
  1 = IF DROP FALSE THEN ;

\ Increment recursion level counter and maintain the maximum.
: rl+ ( -- ) reclev 1+!
  reclev @ reclevmax @ MAX reclevmax ! ;

\ Decrement recursion level counter.
: rl- ( -- ) reclev 1-! ;

: speculate ( -- success-flag )
  rl+                          \ Increment recursion level

  get-unresolved               \ Look for an unresolved spot
  DUP 0= IF
    solutions 1+!
    stopon1st 0= IF
      CR display-grid
    THEN
    INVERT EXIT                \ Problem solved
  THEN

  DUP @                        \ S: saddr\sval
  \ The list of set bits in TOS indicate the possibilities
  \ for the selected spot. Explore these alternatives.
  16 0 DO
    DUP I 2^n DUP >R AND IF
      TRUE 2 PICK tstk-push    \ Insert transaction boundary

      R> 2 PICK
        +ul |visual -ul
        unknowns 1-!           \ Spot provisionally resolved
        !                      \ Un-logged update-spot

      infer IF                 \ No inconsistencies detected
        RECURSE IF             \ Solution found
          stopon1st IF
            2DROP UNLOOP TRUE EXIT
          THEN
        THEN
      THEN

      \ Backtrack up to the last transaction boundary.
      BEGIN tstk-pop UNTIL
      nbt 1+!                  \ Increment #backtracks
    ELSE
      R> DROP
    THEN
  LOOP

  2DROP FALSE                  \ Dead end reached
  rl- ;                        \ Decrement recursion level

: main ( -- )
  inits

  stopon1st IF
    PAGE -cursor display-grid
  THEN

  infer 0= IF
    +cursor
    CR ." No solutions" QUIT
  THEN

  \ From here on, everything that could be inferred is in.
  TRUE TO logtrans

  speculate DROP

  stopon1st IF
    31 15 AT-XY
    CR ." Maximum recursion level: " reclevmax ?
    CR ." Problem solved at level: " reclev ?
  ELSE
    CR solutions ? ." solution(s) found"
  THEN
  CR ." 'countbits' called " ncb 2@ <# #S #> TYPE ."  times"
  CR ." Backtracked " nbt ? ." times"
  +cursor ;

main \ 7 EMIT wasteit


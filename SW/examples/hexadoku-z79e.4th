
\ The interesting thing about this algorithm is that it does
\ not work by looking for a solution. It works by systematic
\ refutation of possibilities leading to constraint violations.
\ Eventually, a solution might emerge--or not. There could be
\ several solutions to a weakly defined problem.
\
\ This code targets Z79Forth (2 byte cell) exclusively. It has
\ several optimizations coded in 6309 machine code.

\ "[Agent Smith] Never send a human to do a machine's job."
\ from "The Matrix" movie, 1999.

\ -------------------------------------------------------------
\ Glossary:

\ grid: a 16 row by 16 columns structure consisting of spots.

\ spot: an unsigned 16 bit integer (stored as a cell) that
\ represents either:
\ - a resolved term for the hexadecimal number residing at
\   the corresponding location. A resolved term is an exact
\   power of two, i.e. only one bit is set for that spot.
\ - or the logical sum of possible values for that spot.
\   This would be the sum of all possibilities, in quantum
\   theory terms. Such a number has more than one bit set.

\ fixed point: a grid cell that is resolved, i.e. has a
\ provisional value that is a power of two (non-zero). Such a
\ cell value cannot be altered by mask application/filtering.

\ transaction: a set of saved states made immediately prior
\ to an original speculative decision and covering all
\ subsequently inferred changes before a dead end situation is
\ detected or a nested speculative decision is made. This is
\ the basis for an undo log buffer (aka transaction stack).

\ A spot having zero for its value indicates a dead end in the
\ current problem resolution state. This program strives to
\ behave so as to avoid that from ever happening.

\ -------------------------------------------------------------
\ "Whereof one cannot speak, thereof one must be silent."
\ Ludwig Wittgentein, "Tractatus Logico-Philosophicus."

\ -------------------------------------------------------------
DECIMAL
MARKER wasteit

\ Following code block borrowed from GNU Forth 0.7.3 vt100.fs.
: pn    BASE @ SWAP DECIMAL 0 U.R BASE ! ;
: ;pn   [CHAR] ; EMIT pn ;
: esc[  #27 EMIT [CHAR] [ EMIT ;
: AT-XY 1+ SWAP 1+ SWAP esc[ pn ;pn [CHAR] H EMIT ;

\ GNU Forth material.
\ : cell/   3 RSHIFT ;
\ : 2cells/ 4 RSHIFT ;

\ Z79Forth material.
  : cell/   1 RSHIFT ;
  : 2cells/ 2 RSHIFT ;

: 16* 4 LSHIFT ;
: 16/mod DUP $F AND SWAP 4 RSHIFT ;
: 1+! 1 SWAP +! ;
: 1-! -1 SWAP +! ;

\ -------------------------------------------------------------
\ Variables and constants.

FALSE VALUE logtrans    \ If NZ log changes to the trans. stack
BL CONSTANT wildc

\ User defined exception numbers.
10 CONSTANT ezgcv              \ Zero grid cell value detected
20 CONSTANT ecvio              \ Constraint violation detected

VARIABLE #unknown

CREATE grid 256 CELLS ALLOT    \ 16x16 is the problem size

\ A transaction is the unit of rollbacks (undos). It is defined
\ as a set of grid saved states between the time we make a
\ speculative choice and the time when an inconsistency is
\ detected or when a nested speculative choice is made
\ (excluded).

4096 CONSTANT tstk-nitems
CREATE tstack tstk-nitems 2* CELLS ALLOT
HERE CONSTANT tstk-bottom
\ Each entry on the transaction stack is:
\ TOS:  saved-bitmask		1 CELL
\ TOS+1: mxxx.xxxx:yyyy.yyyy	1 CELL
\ 	bit #15:	beginning of transaction marker.
\ 	bits #14-8:	xcol: 0..15
\ 	bit #7-0:	yrow: 0..15
VARIABLE tstkp

\ Statistical data support.
VARIABLE reclev                \ Current recursion level
VARIABLE reclevmax             \ Maximum recursion level
CREATE nbt 2 CELLS ALLOT       \ # backtracks (double)
CREATE ncb 2 CELLS ALLOT       \ # calls to countbits (double)
CREATE #zgcv 2 CELLS ALLOT     \ # zero grid cell values dcd
CREATE #cvio 2 CELLS ALLOT     \ # constraint violations dcd

\ -------------------------------------------------------------
\ Bit count utilities.

: d1+! DUP 2@ 1. D+ ROT 2! ;
: 2@ud. 2@ <# #S #> TYPE SPACE ;

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

\ h. is standard equipment in SwiftForth and yet its behavior
\ is different between the 32 and 64 bit implementations.
\ So you will get a warning here, which can safely be ignored.
: h. ( n -- )
  BASE @ >R
  [CHAR] $ EMIT HEX U.
  R> BASE ! ;

\ Compute 2^n fast, i.e. faster than LSHIFT can do it.
\ Note: 'n' is restricted to the [0..15] range.
CREATE exptbl
1     , 2     , 4     , 8     ,
$10   , $20   , $40   , $80   ,
$100  , $200  , $400  , $800  ,
$1000 , $2000 , $4000 , $8000 ,

: 2^n ( n -- 2^n )
  DUP 0 16 WITHIN 0= IF
    CR ." 2^n: illegal argument: "
    h. ABORT
  THEN
  CELLS exptbl + @ ;

\ Contributed by Bob Armstrong.
: pow2? ( n -- f )
  DUP DUP 1- AND 0= SWAP 0<> AND ;

\ -------------------------------------------------------------
\ Incremental grid visualization.

: getxy-from-grid-addr ( saddr - x y )
  grid - cell/ 16/mod ;

: |visual ( val saddr -- val saddr )
  OVER pow2? IF                \ Spot value is known
    OVER 16 0 DO
      DUP I 2^n = IF
        DROP I
        DUP 9 > IF 7 + THEN [CHAR] 0 +
        LEAVE
      THEN
    LOOP
  ELSE         
    wildc
  THEN

  \ S: val\saddr\char-from-val
  \ Return immediately if char==wildc and !pow2?(saddr@)
  \ This corresponds to a situation where a given cell's mask
  \ changes but the spot remains unresolved.
  OVER @ pow2? 0= OVER wildc = AND IF
    DROP EXIT
  THEN

  OVER getxy-from-grid-addr    \ val\saddr\char-from-val\x\y

  SWAP 2* SWAP AT-XY EMIT ;

\ -------------------------------------------------------------
\ Transaction stack handling (undo log).

: cell- 1 CELLS - ;

\ There is a remote possibility that ABORT" will throw an
\ exception. If it does, it will be caught by 'infer'.
\ The same concern applies to 'tstk-pop'.
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

  tstkp @ DUP @ >R             \ R: bitmsk, S: tsktp@
  CELL+ tstkp !

  tstkp @ DUP @ >R             \ R: bitmsk\mX:Y, S: tsktp@
  CELL+ tstkp !

  R> DUP $8000 AND             \ R: bitmsk, S: mX:Y\beg-flg
  SWAP $7FFF AND               \ R: bitmsk, S: beg-flg\X\Y

  DUP 8 RSHIFT SWAP $FF AND    \ R: bitmsk, S: beg-flg\X\Y
  16* + CELLS grid +           \ R: bitmsk, S: beg-flg\saddr
  R> SWAP                      \ S: beg-flg\bitmsk\saddr

  \ Check whether we are going from resolved to unresolved.
  \ If so increment '#unknown' accordingly.
  DUP @ pow2? IF               \ S: beg-flg\bitmsk\saddr
    OVER pow2? 0= IF
      #unknown 1+!
    THEN
  THEN

  |visual ! ;                  \ Implicit update-spot

\ -------------------------------------------------------------
\ Primary way of altering a grid's spot but not the only one!
\ 'speculate' does unregistered grid changes as well.
\ The only changes we can see here are inferred. Undo
\ operations are carried out via 'tstk-pop'.

: update-spot ( val saddr -- )
  2DUP @ = IF                  \ Value not changed
    2DROP EXIT
  THEN

  logtrans IF                  \ Transaction is logged
    FALSE OVER tstk-push
  THEN

  \ This update resolves the spot pointed to by 'saddr'.
  OVER pow2? IF #unknown 1-! THEN

  \ Commit this change to the grid.
  |visual ! ;                  \ S: --

\ -------------------------------------------------------------
\ Initializations.

: char>digit ( char -- char | digitval )
  DUP [CHAR] 0 [CHAR] 9 1+ WITHIN IF [CHAR] 0 - EXIT THEN
  DUP [CHAR] A [CHAR] F 1+ WITHIN IF [CHAR] 7 - THEN ;

: initline ( srcaddr bytecount linenum -- )
  16* CELLS grid +             \ srcaddr\bytecount\tgtaddr
  SWAP 0 DO                    \ srcaddr\tgtaddr
    OVER I + C@ char>digit DUP 16 < IF
      2^n                      \ srcaddr\tgtaddr\2^<digit>
      OVER !                   \ srcaddr\tgtaddr
      CELL+                    \ srcaddr\tgtaddr-next
      #unknown 1-!
    ELSE
      [CHAR] : <> IF CELL+ THEN
    THEN
  LOOP
  2DROP ;

: inits ( -- )
  256 #unknown !
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
 
  \ Original design (1.25M backtracks required).
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

  \ Elektor data set from the May/June, 2023 issue.
\ S" E2.A:0..B:.F.C:649." 0  initline
\ S" .C.F:158.:...0:.BA7" 1  initline
\ S" ..1.:..4.:....:.8.2" 2  initline
\ S" 834.:..CD:.7BE:.01." 3  initline

\ S" ..F8:....:...3:..29" 4  initline
\ S" 7.23:..1.:..9B:D..A" 5  initline
\ S" .6..:.D.8:.AE.:5..." 6  initline
\ S" .1..:5..6:.8.D:...." 7  initline

\ S" .7..:A..3:.E.6:...." 8  initline
\ S" .D..:.6.2:.B3.:8..." 9  initline
\ S" 3.96:..5.:..7F:1..0" 10 initline
\ S" ..05:....:...9:..76" 11 initline

\ S" 09E.:..2A:.3F5:.C6." 12 initline
\ S" ..7.:..6.:....:.5.3" 13 initline
\ S" .8.C:F73.:...1:.D0B" 14 initline
\ S" 5F.4:8..0:.6.2:A7E." 15 initline

  \ Elektor data set from the July/August, 2023 issue.
\ S" D.16:0.27:.A..:...B" 0  initline
\ S" .3..:....:0182:7..." 1  initline
\ S" 5.F.:...4:3B.D:..0A" 2  initline
\ S" A...:.35.:...F:..4." 3  initline

\ S" 3...:4..1:9C..:E..." 4  initline
\ S" ...1:.B..:6...:.C8." 5  initline
\ S" 6..5:..90:A8..:.DF." 6  initline
\ S" 0.A.:C.D.:..1.:4567" 7  initline

\ S" .D6.:84F.:.E..:2..." 8  initline
\ S" 2E8.:5.0.:74..:61.." 9  initline
\ S" .F..:...6:..2B:..5." 10 initline
\ S" .504:....:..9.:C.E8" 11 initline

\ S" .6..:9..3:C7.0:...." 12 initline
\ S" ....:.542:.9..:...." 13 initline
\ S" ..52:.0EA:..D8:..B." 14 initline
\ S" 8.9.:...F:...4:...6" 15 initline

  \ Elektor data set from the Holiday Circuits #4, 2023 issue.
  \ Solved by inference only. No speculation required!!!
\ S" F02C:D1..:..48:E75A" 0  initline
\ S" .3.9:E4F.:.5CD:6.8." 1  initline
\ S" 4A6.:....:....:.31D" 2  initline
\ S" 8...:9..A:6..1:...F" 3  initline

\ S" ..AE:..0F:B9..:CD.." 4  initline
\ S" 063.:1.47:DF.E:.5B9" 5  initline
\ S" .B.F:.D.5:C.2.:3.7." 6  initline
\ S" ..C.:.3..:..8.:.F.." 7  initline

\ S" ..D.:.A..:..6.:.0.." 8  initline
\ S" .C.2:.8.E:0.B.:1.A." 9  initline
\ S" 79B.:4.63:81.2:.CE5" 10 initline
\ S" ..01:..9D:47..:8B.." 11 initline

\ S" A...:3..2:F..B:...7" 12 initline
\ S" B14.:....:....:.89C" 13 initline
\ S" .E.7:6BD.:.A90:4.2." 14 initline
\ S" D290:7F..:..EC:5A6B" 15 initline

  \ Elektor data set from the September/October, 2023 issue.
  \ Solved by inference only. No speculation required!!!
\ S" ..E3:FB.5:8.26:40.." 0  initline
\ S" B...:.EA.:.59.:...3" 1  initline
\ S" 4...:.209:E7B.:...D" 2  initline
\ S" .7..:....:....:..5." 3  initline

\ S" 9.BE:74..:..81:32.C" 4  initline
\ S" ..46:9.CA:0B.7:DF.." 5  initline
\ S" 7.AD:8.B.:.4.2:56.1" 6  initline
\ S" 58..:D02.:.36A:..B4" 7  initline

\ S" 63..:B57.:.EAC:..80" 8  initline
\ S" 8.01:E.3.:.6.5:9B.A" 9  initline
\ S" ..DB:4.62:3F.9:C7.." 10 initline
\ S" C.27:0A..:..4D:F3.5" 11 initline

\ S" .9..:....:....:..7." 12 initline
\ S" E...:.943:52C.:...6" 13 initline
\ S" 0...:.7F.:.AD.:...9" 14 initline
\ S" ..84:5C.B:7.F0:AD.." 15 initline

  \ Transaction stack initialization.
  tstk-bottom tstkp !
  FALSE TO logtrans

  \ Statistical data initialization.
  0. ncb 2!
  0. nbt 2!
  0. #zgcv 2!                  \ Initialize exception counts
  0. #cvio 2!
  0. reclev ! reclevmax ! ;    \ and recursion level counts

\ -------------------------------------------------------------
\ Visualization.

\ Underline character rendition on.
: +ul ( -- )
  #27 EMIT ." [4m" ;

\ Underline character rendition off.
: -ul ( -- )
  #27 EMIT ." [m" ;

\ Turn off the cursor (VT200 control sequence).
: -cursor ( -- )
  #27 EMIT ." [?25l" ;

\ Turn on the cursor (VT200 control sequence).
: +cursor ( -- )
  #27 EMIT ." [?25h" ;

: mask>char ( mask -- char )
  DUP pow2? IF                 \ S: mask\nbits
    16 0 DO
      DUP I 2^n = IF
        DROP I UNLOOP
        DUP 9 > IF  7 + THEN [CHAR] 0 +
        EXIT
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
\ Four by four exclusion/filtering.

\ No side effects.
: 4x4-getmask ( xcol yrow -- mask | exception-triggered )
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
          #cvio d1+!
          ecvio THROW       \ Constraint violation detected
        THEN

        \ xcol\yrow\mask\val\check\val
        OR               \ S: xcol\yrow\mask\val\(check|val)
        -rot             \ S: xcol\yrow\(check|val)\mask\val
        INVERT AND       \ S: xcol\yrow\(check|val)\(mask&~val)
      ELSE
        DROP
      THEN
    LOOP
  LOOP
  \ S: xcol\yrow\check\mask
  NIP NIP NIP ;

: 4x4-setmask ( xcol yrow mask -- | exception-triggered )
  \ If 'mask' is zero, it means that all cells in that 4x4
  \ quadrant are resolved (fixed points). Just exit should
  \ such a condition occur.
  ?DUP 0= IF 2DROP EXIT THEN

  -rot                         \ mask\xcol\yrow
  4 0 DO                       \ J has dy
    4 0 DO                     \ I has dx
      OVER I +                 \ Absolute col#
      OVER J + 16* +
      CELLS grid +             \ S: mask\xcol\yrow\saddr
      DUP @ DUP pow2? IF
        2DROP
      ELSE
        \ mask\xcol\yrow\saddr\sval
        4 PICK AND           \ S: mask\xcol\yrow\saddr\sval-new
        ?DUP IF
          SWAP update-spot
        ELSE
          #zgcv d1+!
          ezgcv THROW       \ Zero grid cell value detected
        THEN
      THEN
      \ S: mask\xcol\yrow
    LOOP
  LOOP
  DROP 2DROP ;

\ 4x4 block logic: either a spot is known or the list
\ of alternatives must exclude all known spots values.
: 4x4-reduce-allquads ( -- | exception-triggered )
  4 0 DO              \ Iterate over quadrant rows
    4 0 DO            \ Iterate over quadrant columns
      I 4 * J 4 *
      2DUP 4x4-getmask
      ( S: xcol#\yrow#\new-possibly-zero-mask ) 4x4-setmask
    LOOP
  LOOP ;

\ -------------------------------------------------------------
\ Horizontal exclusion/filtering.

\ ANS94 3.2.3.3 Return stack:
\ A program shall not access from within a DO-LOOP values
\ placed on the return stack before the loop was entered.
\ Note: this is enforced in SwiftForth but not in GForth.

\ No side effects.
: horiz-getmask ( yrow -- mask | exception-triggered )
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
        #cvio d1+!
        ecvio THROW         \ Constraint violation detected
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
  NIP NIP ;

: horiz-setmask ( yrow mask -- | exception-triggered )
  \ If 'mask' is zero. just return a success indication.
  ?DUP 0= IF DROP EXIT THEN

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
      ELSE
        #zgcv d1+!
        ezgcv THROW         \ Zero grid cell value detected
      THEN
    THEN
    \ S: mask\saddr
    CELL+
  LOOP
  2DROP ; 

\ -------------------------------------------------------------
\ Vertical exclusion/filtering.

\ No side effects.
: vert-getmask ( xcol -- mask | exception-triggered )
  CELLS grid +                 \ Start of column address
  0                            \ Sanity check
  $FFFF                        \ Initial mask
  16 0 DO                      \ Iterate over rows
    \ scol-addr\check\mask
    2 PICK I 16* CELLS + @ DUP pow2? IF
      \ S: scol-addr\check\mask\val
      ROT OVER \ S: scol-addr\mask\val\check\val
      2DUP AND \ S: scol-addr\mask\val\check\val\(check&val)

      IF                       \ Bit is already set!!!
        #cvio d1+!
        ecvio THROW         \ Constraint violation detected
      THEN

      \ scol-addr\mask\val\check\val
      OR                    \ S: scol-addr\mask\val\(check|val)
      -rot                  \ S: scol-addr\(check|val)\mask\val
      INVERT AND
    ELSE
      DROP
    THEN
  LOOP
  \ S: srow-addr\check\mask
  NIP NIP ;

: vert-setmask ( xcol mask -- | exception-triggered )
  \ If 'mask' is zero. just return a success indication.
  ?DUP 0= IF DROP EXIT THEN

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
      ELSE
        #zgcv d1+!
        ezgcv THROW         \ Zero grid cell value detected
      THEN
    THEN
    \ S: mask\saddr
    16 CELLS +
  LOOP
  2DROP ;

\ We do not not have to check for inconsistencies at all.
\ They will result in exceptions being raised by 'update-spot'.
: reduceall ( -- | exception-triggered )
  4x4-reduce-allquads

  16 0 DO
    I DUP horiz-getmask horiz-setmask
    I DUP vert-getmask  vert-setmask
  LOOP ;

\ -------------------------------------------------------------
\ Speculation.

: infer ( -- success-flag )
  256                          \ '#unknown' worst case scenario
  BEGIN
    ['] reduceall CATCH >R     \ S: unkmax R: exc# | 0
    R@ 0<> R@ ezgcv <> AND R@ ecvio <> AND IF
      \ The exception caught is not one of ours.
      0 16 AT-XY  +cursor
      ." infer: caught exception " R> .
      CR QUIT
    THEN
    \ S: unkmax R: exc# | 0
    R> IF DROP FALSE EXIT THEN  \ A known exception occurred
    ( S: unkmax) #unknown @ >
  WHILE
    #unknown @
  REPEAT
  TRUE ;

\ "[Gordon Gekko]: The point is ladies and gentlemen that
\ greed, for lack of a better word, is good." from the "Wall
\ Street" movie, 1987.
: get-unresolved ( -- grid-cell-addr | FALSE )
  grid DUP @ countbits          \ S: minp\minp@#bits
  OVER CELL+                    \ S: minp\minp@#bits\curp

  255 0 DO
    DUP @ countbits        \ S: minp\minp@#bits\curp\curp@#bits

    \ The newly selected minimum bit count cannot ever be 1.
    DUP 1 = IF
      DROP
    ELSE
      \ minp@#bits can be 1, indicating a resolved spot.
      \ If it is, accept anything but 1 as a new minimum.
      2 PICK 1 = IF        \ S: minp\minp@#bits\curp\curp@#bits
        2nip 2DUP
      THEN

      \ minp\minp@#bits\curp\curp@#bits
      DUP 2 = IF                \ 2 as a good enough minimum
        2nip OVER LEAVE         \ S: curp\curp@#bits\curp
      THEN

      DUP 3 PICK < IF      \ S: minp\minp@#bits\curp\curp@#bits
        2nip OVER               \ S: curp\curp@#bits\curp
      ELSE
        DROP
      THEN
    THEN

    CELL+                       \ S: minp\minp@#bits\curp
  LOOP
  DROP                          \ S: minp\minp@#bits

  \ If the minimum bit count is 1 the problem is solved.
  1 = IF DROP FALSE THEN ;

\ Increment recursion level counter and maintain the maximum.
: rl+ ( -- ) reclev 1+!
  reclev @ reclevmax @ MAX reclevmax ! ;

\ Decrement recursion level counter.
: rl- ( -- ) reclev 1-! ;

: emergency-exit ( ... -- )
  \ Drain the data stack.
  BEGIN DEPTH WHILE DROP REPEAT
  CR QUIT ;                    \ Clear the return stack

: speculate ( -- success-flag )
  rl+                          \ Increment recursion level

  get-unresolved               \ Look for an unresolved spot
  DUP 0= IF                    \ Problem solved
    INVERT                     \ Return TRUE
    EXIT
  THEN

  DUP @                        \ S: saddr\sval

  \ The list of set bits in TOS indicate the possibilities
  \ for the selected spot. Explore these alternatives.
  16 0 DO
    DUP I 2^n DUP >R AND IF
      TRUE 2 PICK tstk-push    \ Insert transaction boundary

      R> 2 PICK
        +ul |visual -ul
        #unknown 1-!           \ Spot provisionally resolved
        !                      \ Un-logged update-spot

      infer
      IF                       \ No inconsistencies detected
        RECURSE IF
          2DROP UNLOOP TRUE EXIT
        THEN
      THEN

      \ Backtrack up to the last transaction boundary.
      BEGIN tstk-pop UNTIL
      nbt d1+!                 \ Increment #backtrack

    ELSE
      R> DROP
    THEN
  LOOP

  rl-                          \ Decrement recursion level
  2DROP FALSE ;                \ Dead end reached

: main ( -- )
  inits

  PAGE -cursor display-grid

  infer 0= IF
    +cursor
    CR ." No solutions"
    emergency-exit
  THEN

  \ From here on, everything that could be inferred is in.
  \ Enable transaction logging.
  TRUE TO logtrans

  speculate DROP
  0 16 AT-XY
     ." Maximum recursion level: " reclevmax ?
  CR ." Problem solved at level: " reclev ?
  CR ." 'countbits' called " ncb 2@ud. ." times"
  CR ." Backtracked " nbt 2@ud. ." times"
  +cursor

  CR #cvio 2@ud. ." constraint violations"
  CR #zgcv 2@ud. ." zero grid cell values" ;

main 7 EMIT


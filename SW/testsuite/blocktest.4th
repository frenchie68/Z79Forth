\ The original code has been amended to check for 79-STANDARD
\ correctness rather than for ANSI compliance.

\ This program was written by Steve R. Palmer in 2015, with
\ contributions from others where indicated, and is in the
\ public domain - it can be distributed and/or modified in any
\ way but please retain this notice.

\ This program is distributed in the hope that it will be
\ useful, but WITHOUT ANY WARRANTY; without even the implied
\ warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
\ PURPOSE.

\ The tests are not claimed to be comprehensive or correct

\ Version 0.1 23 October 2015  First Version
\ Version 0.2 15 November 2015 Updated after feedback from
\ Gerry Jackson

\ -------------------------------------------------------------
\ The tests are based on John Hayes test program for the core
\ word set. The default BASE is assumed to be decimal.
\
\ Words tested in this file are:
\ BLK BLOCK BUFFER FLUSH LOAD SAVE-BUFFERS UPDATE EMPTY-BUFFERS
\ LIST SCR THRU


\ Assumptions and dependencies:
\ - tester.4th has been loaded prior to this file

TESTING Block word set%

DECIMAL

\ Define these constants from the system documentation
\ provided.  WARNING: The contents of the test blocks will be
\ destroyed by this test. The blocks tested will be in the
\ range:
\   FIRST-TEST-BLOCK <= u < LIMIT-TEST-BLOCK
\ The tests need at least 2 test blocks in the range to
\ complete.
130 CONSTANT FIRST-TEST-BLOCK
140 CONSTANT LIMIT-TEST-BLOCK \ one beyond the last
16 CONSTANT BITS/CELL

: CHK1 FIRST-TEST-BLOCK LIMIT-TEST-BLOCK U< UNLESS
    ." Error: test block range not identified" CR ABORT
  THEN ;
CHK1

LIMIT-TEST-BLOCK FIRST-TEST-BLOCK - CONSTANT TEST-BLOCK-COUNT
: CHK2 TEST-BLOCK-COUNT 2 U< IF
    ." Error: at least 2 test blocks are required" CR ABORT
  THEN ;
CHK2

TESTING Random Number Utilities%

\ The block tests make extensive use of random numbers to
\ select blocks to test and to set the contents of the block.
\ It also makes use of a Hash code to ensure the integrity of
\ the blocks against unexpected changes.

\ == Memory Walk tools ==

: @++ ( a-addr -- a-addr+4 a-addr@ )
    DUP CELL+ SWAP @ ;
: !++ ( x a-addr -- a-addr+4 )
    TUCK ! CELL+ ;
: C@++ ( c-addr -- c-addr;char+ c-addr@ )
    DUP CHAR+ SWAP C@ ;
: C!++ ( char c-addr -- c-addr+1 )
    TUCK ! CHAR+ ;

\ == Random Numbers ==

\ Initialize the PRNG seed seed.
VARIABLE seed  23741 seed !

\ The linear congruential pseudo random number generator itself
: RANDOM seed @ 613 * 5179 + DUP seed ! ;
: RANDOM-BOUND ( lower upper -- rnd )
  OVER - RANDOM ABS SWAP MOD + ;

: BLOCK-RND ( -- rnd )                RANDOM ;
: BLOCK-RANDOM ( lower upper -- rnd ) RANDOM-BOUND ;

: RND-TEST-BLOCK ( -- blk )
    FIRST-TEST-BLOCK LIMIT-TEST-BLOCK BLOCK-RANDOM ;
\ PostCondition:
\ T{ RND-TEST-BLOCK FIRST-TEST-BLOCK LIMIT-TEST-BLOCK WITHIN
\   -> TRUE }T





\ : foo CR 30000 0 DO
\     RND-TEST-BLOCK DUP FIRST-TEST-BLOCK LIMIT-TEST-BLOCK
\     WITHIN IF DROP
\     ELSE U. DROP ABORT
\     THEN
\   LOOP
\ Two distinct random test blocks
: 2RND-TEST-BLOCKS ( -- blk1 blk2 )
  RND-TEST-BLOCK BEGIN  \ blk1
    RND-TEST-BLOCK      \ blk1 blk2
    2DUP =              \ blk1 blk2 blk1==blk2
    WHILE               \ blk1 blk1
      DROP              \ blk1
    REPEAT ;            \ blk1 blk2

\ PostCondition: T{ 2RND-TEST-BLOCKS = -> FALSE }T
\ : bar 30000 0 DO
\     2RND-TEST-BLOCKS = IF
\       ABORT
\     THEN
\   LOOP ;

\ first random test block in a sequence of length u
: RND-TEST-BLOCK-SEQ ( u -- blks )
  FIRST-TEST-BLOCK LIMIT-TEST-BLOCK ROT 1- - BLOCK-RANDOM ;

: ELF-HASH-ACCUMULATE ( hash c-addr u -- hash )
    >R SWAP R> 0 DO                  \ c-addr h
      4 LSHIFT                       \ c-addr h<<=4
      SWAP C@++ ROT +                \ c-addr' h+=*s
      DUP $F000 AND                  \ c-addr' h high=h&$F000
      DUP IF                         \ c-addr' h high
        DUP >R 12 RSHIFT XOR R>      \ c-addr' h^=high>>12 high
      THEN                           \ c-addr' h high
      INVERT AND                     \ c-addr' h&=~high
    LOOP NIP ;

: ELF-HASH ( c-addr u -- hash )
    0 -ROT ELF-HASH-ACCUMULATE ;
TESTING BLOCK (read-only mode)%

\ BLOCK signature
T{ RND-TEST-BLOCK BLOCK DUP ALIGNED = -> TRUE }T

\ BLOCK accepts all blocks in the test range
: BLOCK-ALL ( blk2 blk1 -- )
    DO
        I BLOCK DROP
    LOOP ;
T{ LIMIT-TEST-BLOCK FIRST-TEST-BLOCK BLOCK-ALL -> }T

\ BLOCK twice on same block returns the same value
T{ RND-TEST-BLOCK DUP BLOCK SWAP BLOCK = -> TRUE }T

\ BLOCK twice on distinct block numbers
\ may or may not return the same value!
\ Nothing to test

\ -------------------------------------------------------------
TESTING BUFFER (read-only mode)%

\ Although it is not in the spirit of the specification,
\ a compliant definition of BUFFER would be
\ : BUFFER BLOCK ;
\ So we can only repeat the tests for BLOCK ...
\ FLA note: this is not the case. BUFFER allows blocks to be
\ written to mass storage without having to be read first.

\ BUFFER signature
T{ RND-TEST-BLOCK BUFFER DUP ALIGNED = -> TRUE }T

\ BUFFER accepts all blocks in the test range
: BUFFER-ALL ( blk2 blk1 -- )
    DO
        I BUFFER DROP
    LOOP ;
T{ LIMIT-TEST-BLOCK FIRST-TEST-BLOCK BUFFER-ALL -> }T

\ BUFFER twice on the same block returns the same value
T{ RND-TEST-BLOCK DUP BUFFER SWAP BUFFER = -> TRUE }T

\ BUFFER twice on distinct block numbers
\ may or may not return the same value!
\ Nothing to test

\ Combinations with BUFFER
T{ RND-TEST-BLOCK DUP BUFFER SWAP BLOCK = -> TRUE }T

\ -------------------------------------------------------------
TESTING Read and Write access with UPDATE and FLUSH%

\ Ideally, we'd like to be able to test the persistence across
\ power cycles of the writes, but we can't do that in a simple
\ test. The tests below could be fooled by a large buffers
\ store and a tricky FLUSH but what else are you going to do?
\ Signatures
T{ RND-TEST-BLOCK BLOCK DROP UPDATE -> }T
T{ FLUSH -> }T

: BLANK-BUFFER ( blk -- blk-addr )
  BUFFER DUP 1024 BL FILL ;

\ Test R/W of a Simple Blank Random Block
: TUF-A T{ RND-TEST-BLOCK         \ blk
   DUP BLANK-BUFFER               \ blk blk-addr1
   1024 ELF-HASH                  \ blk hash
   UPDATE FLUSH                   \ blk hash
   SWAP BLOCK                     \ hash blk-addr2
   1024 ELF-HASH = -> TRUE }T ; TUF-A \ ." TUF-A "

\ Boundary Test: Modify first character
: TUF-B T{ RND-TEST-BLOCK          \ blk
   DUP BLANK-BUFFER                \ blk blk-addr1
   [CHAR] \ OVER C!                \ blk blk-addr1
   1024 ELF-HASH                   \ blk hash
   UPDATE FLUSH                    \ blk hash
   SWAP BLOCK                      \ hash blk-addr2
   1024 ELF-HASH = -> TRUE }T ;
TUF-B \ ." TUF-B "

\ Boundary Test: Modify last character
: TUF-C T{ RND-TEST-BLOCK          \ blk
   DUP BLANK-BUFFER                \ blk blk-addr1
   [CHAR] \ OVER 1023 CHARS + C!   \ blk blk-addr1
   1024 ELF-HASH                   \ blk hash
   UPDATE FLUSH                    \ blk hash
   SWAP BLOCK                      \ hash blk-addr2
   1024 ELF-HASH = -> TRUE }T ;
TUF-C \ ." TUF-C "
\ Boundary Test: First and Last (and all other) blocks in
\ the test range
1024 8 * BITS/CELL / CONSTANT CELLS/BLOCK

: PREPARE-RND-BLOCK ( hash blk -- hash' )
  BUFFER DUP                     \ hash blk-addr blk-addr
  CELLS/BLOCK 0 DO               \ hash blk-addr blk-addr[i]
    BLOCK-RND OVER ! CELL+     \ hash blk-addr blk-addr[i+1]
  LOOP DROP                      \ hash blk-addr
  1024 ELF-HASH-ACCUMULATE ;     \ hash'

: WRITE-RND-BLOCKS-WITH-HASH ( blk2 blk1 -- hash )
  0 -ROT DO                      \ hash
    I PREPARE-RND-BLOCK UPDATE   \ hash'
  LOOP ;                         \ hash'

: READ-BLOCKS-AND-HASH ( blk2 blk1 -- hash )
  0 -ROT DO                            \ hash(i)
    I BLOCK 1024 ELF-HASH-ACCUMULATE   \ hash(i+1)
  LOOP ;                               \ hash

T{ LIMIT-TEST-BLOCK FIRST-TEST-BLOCK
     WRITE-RND-BLOCKS-WITH-HASH FLUSH
   LIMIT-TEST-BLOCK FIRST-TEST-BLOCK
     READ-BLOCKS-AND-HASH =
   -> TRUE }T

: TUF1 ( xt blk -- hash )
    DUP BLANK-BUFFER               \ xt blk blk-addr1
    1024 ELF-HASH                  \ xt blk hash
    ROT EXECUTE                    \ blk hash
    SWAP BLOCK                     \ hash blk-addr2
    1024 ELF-HASH = ;              \ TRUE

\ Double UPDATE make no difference
: TUF1-1 ( -- ) UPDATE UPDATE FLUSH ;
T{ FIND TUF1-1 RND-TEST-BLOCK TUF1 -> TRUE }T

\ Double FLUSH make no difference
: TUF1-2 ( -- ) UPDATE FLUSH FLUSH ;
T{ FIND TUF1-2 RND-TEST-BLOCK TUF1 -> TRUE }T

\ FLUSH only saves UPDATEd buffers
: TUF-D T{ RND-TEST-BLOCK              \ blk
   0 OVER PREPARE-RND-BLOCK            \ blk hash
   UPDATE FLUSH                        \ blk hash
   OVER 0 SWAP PREPARE-RND-BLOCK DROP  \ blk hash
   FLUSH ( with no preliminary UPDATE) \ blk hash
   SWAP BLOCK 1024 ELF-HASH
   = -> TRUE }T ; TUF-D \ ." TUF-D "


\ UPDATE only marks the current block buffer as dirty.
\ This test needs at least 2 distinct buffers, though this is
\ not a requirement of the language specification. If 2
\ distinct buffers are not returned, then the tests quits with
\ a trivial pass
: TUF2
  \ xt blk1 blk2 -- hash1'' hash2'' hash1' hash2' hash1 hash2
  OVER BUFFER OVER BUFFER = IF \ test needs 2 distinct buffers
    2DROP DROP 0 0 0 0 0 0     \ Dummy result
  ELSE
    OVER 0 SWAP PREPARE-RND-BLOCK
      UPDATE \ xt blk1 blk2 hash1
    OVER 0 SWAP PREPARE-RND-BLOCK
      UPDATE \ xt blk1 blk2 hash1 hash2
    2>R                            \ xt blk1 blk2
    FLUSH                          \ xt blk1 blk2
    OVER 0 SWAP PREPARE-RND-BLOCK  \ xt blk1 blk2 hash1'
    OVER 0 SWAP PREPARE-RND-BLOCK  \ xt blk1 blk2 hash1' hash2'
    2>R                            \ xt blk1 blk2
    ROT EXECUTE                    \ blk1 blk2
    FLUSH                          \ blk1 blk2
    SWAP BLOCK 1024 ELF-HASH       \ blk2 hash1''
    SWAP BLOCK 1024 ELF-HASH       \ hash1'' hash2''
    2R> 2R> \ hash1'' hash2'' hash1' hash2' hash1 hash2
  THEN ;

: 2= ( x1 x2 x3 x4 -- flag )
    ROT = -ROT = AND ;
: TUF2-0 ( blk1 blk2 -- blk1 blk2 ) ;   \ no updates
T{ FIND TUF2-0 2RND-TEST-BLOCKS TUF2    \ run test procedure
   2SWAP 2DROP 2= -> TRUE }T      \ compare expected and actual
\ ." TUF2-0 "
: TUF2-1 ( blk1 blk2 -- blk1 blk2 )     \ update blk1 only
  OVER BUFFER DROP UPDATE ;
: TUF2-1A T{ ['] TUF2-1 2RND-TEST-BLOCKS \ run test procedure
  TUF2 SWAP DROP SWAP DROP 2= -> TRUE }T ;
TUF2-1A \ ." TUF2-1A "
: TUF2-2 ( blk1 blk2 -- blk1 blk2 )     \ update blk2 only
  DUP BUFFER DROP UPDATE ;
: TUF2-2A T{ ['] TUF2-2 2RND-TEST-BLOCKS \ run test procedure
  TUF2 DROP ROT DROP SWAP 2= -> TRUE }T ;
TUF2-2A \ ." TUF2-2A "

: TUF2-3 ( blk1 blk2 -- blk1 blk2 )     \ update blk1 and blk2
    TUF2-1 TUF2-2 ;
T{ FIND TUF2-3 2RND-TEST-BLOCKS TUF2    \ run test procedure
   2DROP 2= -> TRUE }T

\ FLUSH and then UPDATE is ambiguous and untestable

\ -------------------------------------------------------------
TESTING SAVE-BUFFERS%

\ In principle, all the tests above can be repeated with
\ SAVE-BUFFERS instead of FLUSH. However, only the full random
\ test is repeated...
T{ LIMIT-TEST-BLOCK FIRST-TEST-BLOCK WRITE-RND-BLOCKS-WITH-HASH
     SAVE-BUFFERS
   LIMIT-TEST-BLOCK FIRST-TEST-BLOCK READ-BLOCKS-AND-HASH =
   -> TRUE }T

\ FLUSH and then SAVE-BUFFERS is harmless but undetectable
\ SAVE-BUFFERS and then FLUSH is undetectable

\ Unlike FLUSH, SAVE-BUFFERS then BUFFER/BLOCK
\ returns the original buffer address
T{ RND-TEST-BLOCK DUP BLANK-BUFFER
   SAVE-BUFFERS        SWAP BUFFER = -> TRUE }T
T{ RND-TEST-BLOCK DUP BLANK-BUFFER
   UPDATE SAVE-BUFFERS SWAP BUFFER = -> TRUE }T
T{ RND-TEST-BLOCK DUP BLANK-BUFFER
   SAVE-BUFFERS        SWAP BLOCK  = -> TRUE }T
T{ RND-TEST-BLOCK DUP BLANK-BUFFER
   UPDATE SAVE-BUFFERS SWAP BLOCK  = -> TRUE }T

\ -------------------------------------------------------------
TESTING BLK%

\ Signature
T{ BLK DUP ALIGNED = -> TRUE }T

\ None of the words considered so far effect BLK
T{ BLK @ RND-TEST-BLOCK BUFFER DROP BLK @ = -> TRUE }T
T{ BLK @ RND-TEST-BLOCK BLOCK  DROP BLK @ = -> TRUE }T
T{ BLK @ UPDATE                     BLK @ = -> TRUE }T
T{ BLK @ FLUSH        BLK @ = -> TRUE }T
T{ BLK @ SAVE-BUFFERS BLK @ = -> TRUE }T

\ -------------------------------------------------------------
TESTING LOAD%

\ Signature: n LOAD --> blank screen
: TUF3 T{ RND-TEST-BLOCK DUP BLANK-BUFFER DROP UPDATE FLUSH
  LOAD -> }T ;
TUF3 \ ." TUF3 "
: TUF4 T{ BLK @ RND-TEST-BLOCK DUP BLANK-BUFFER DROP UPDATE
  FLUSH LOAD BLK @ = -> TRUE }T ;
TUF4 \ ." TUF4 "
: WRITE-BLOCK ( blk c-addr u -- )
  ROT BLANK-BUFFER SWAP CHARS CMOVE UPDATE FLUSH ;

\ blk: u; blk LOAD
: TL1 ( u blk -- )
  SWAP 0 <# #S #> WRITE-BLOCK ;
: TUF5 T{ BLOCK-RND RND-TEST-BLOCK 2DUP TL1 LOAD =
  -> TRUE }T ;
\ Boundary Test: FIRST-TEST-BLOCK
: TUF6 T{ BLOCK-RND FIRST-TEST-BLOCK 2DUP TL1 LOAD =
  -> TRUE }T ;              TUF6 \ ." TUF6 "
\ Boundary Test: LIMIT-TEST-BLOCK-1
: TUF7 T{ BLOCK-RND LIMIT-TEST-BLOCK 1- 2DUP TL1 LOAD =
  -> TRUE }T ;              TUF7 \ ." TUF7 "
: WRITE-AT-END-OF-BLOCK ( blk c-addr u -- )
  ROT BLANK-BUFFER
  OVER 1024 SWAP - CHARS +
  SWAP CHARS CMOVE UPDATE FLUSH ;

\ Boundary Test: End of Buffer
: TL2 ( u blk -- )
  SWAP 0 <# #S #> WRITE-AT-END-OF-BLOCK ;
: TUF8 T{ BLOCK-RND RND-TEST-BLOCK 2DUP TL2 LOAD =
  -> TRUE }T ;                TUF8 \ ." TUF8 "
\ LOAD updates BLK
\ u: "BLK @"; u LOAD
: TL3 ( blk -- )
  S" BLK @" WRITE-BLOCK ;

: TUF9 T{ RND-TEST-BLOCK DUP TL3 DUP LOAD = -> TRUE }T ;
TUF9 \ ." TUF9 "

\ EVALUATE can nest with LOAD
\ u: "BLK @"; S" u LOAD" EVALUATE
: TL5 ( blk -- c-addr u ) S>D <#
    [CHAR] D HOLD
    [CHAR] A HOLD
    [CHAR] O HOLD
    [CHAR] L HOLD
    BL HOLD
  #S #> ;                    \ c-addr u

\ Nested LOADs
\ u2: "BLK @"; u1: "LOAD u2"; u1 LOAD
: TL6 ( blk1 blk2 -- )
    DUP TL3                    \ blk1 blk2
    TL5 WRITE-BLOCK ;
: TUF10 T{ 2RND-TEST-BLOCKS 2DUP TL6 SWAP LOAD = -> TRUE }T ;
TUF10 \ ." TUF10 "

\ LOAD changes the current block that is effected by UPDATE
\ This test needs at least 2 distinct buffers, though this is
\ not a \ requirement of the language specification. If 2
\ distinct buffers are not returned, then the tests quits with
\ an error condition.
: TL7 ( blk1 blk2 -- u1 u2 rnd2 blk2-addr rnd1' rnd1 )
  OVER BUFFER OVER BUFFER = IF  \ test needs 2 distinct buffers
    2DROP 0 0 0 0 0 0           \ Dummy result
  ELSE
    OVER BLOCK-RND DUP ROT TL1 >R   \ blk1 blk2
    DUP S" SOURCE DROP" WRITE-BLOCK \ blk1 blk2
    \ change blk1 to a new rnd, but don't UPDATE
    OVER BLANK-BUFFER           \ blk1 blk2 blk1-addr
    BLOCK-RND DUP >R            \ blk1 blk2 blk1-addr rnd1'
    0 <# #S #>                  \ blk1 blk2 blk1-addr c-addr u
    ROT SWAP CHARS CMOVE        \ blk1 blk2
    \ Now LOAD blk2
    DUP LOAD DUP >R             \ blk1 blk2 blk2-addr

    \ Write a new blk2
    DUP 1024 BL FILL            \ blk1 blk2 blk2-addr
    BLOCK-RND DUP >R            \ blk1 blk2 blk2-addr rnd2
    0 <# #S #>                  \ blk1 blk2 blk2-addr c-addr u
    ROT SWAP CHARS CMOVE        \ blk1 blk2
   \ Following UPDATE should refer to the LOADed blk2, not blk1
    UPDATE FLUSH                \ blk1 blk2
    \ Finally, load both blocks then collect all results
    LOAD SWAP LOAD              \ u2 u1
    R> R> R> R>               \ u2 u1 rnd2 blk2-addr rnd1' rnd1
  THEN ;
T{ 2RND-TEST-BLOCKS TL7         \ run test procedure
   SWAP DROP SWAP DROP          \ u2 u1 rnd2 rnd1
   2= -> TRUE }T

\ I would expect LOAD to work on the contents of the buffer
\ cache and not the block device, but the specification doesn't
\ say. Similarly, I would not expect LOAD to FLUSH the buffer
\ cache, but the specification doesn't say so.

\ -------------------------------------------------------------
TESTING LIST and SCR%

\ Signatures
T{ SCR DUP ALIGNED = -> TRUE }T
\ LIST signature is test implicitly in the following tests...

: TLS1 ( blk -- )
  S" Should show a (mostly) blank screen" WRITE-BLOCK ;
T{ RND-TEST-BLOCK DUP TLS1 DUP LIST SCR @ = -> TRUE }T

\ Boundary Test: FIRST-TEST-BLOCK
: TLS2 ( blk -- )
  S" List of the First test block" WRITE-BLOCK ;
T{ FIRST-TEST-BLOCK DUP TLS2 LIST -> }T

\ Boundary Test: LIMIT-TEST-BLOCK
: TLS3 ( blk -- )
  S" List of the Last test block" WRITE-BLOCK ;
T{ LIMIT-TEST-BLOCK 1- DUP TLS3 LIST -> }T
\ Boundary Test: End of Screen
: TLS4 ( blk -- )
  S" End of Screen" WRITE-AT-END-OF-BLOCK ;
T{ RND-TEST-BLOCK DUP TLS4 LIST -> }T

\ BLOCK, BUFFER, UPDATE et al don't change SCR
: TLS5 ( blk -- )
  S" Should show another (mostly) blank screen" WRITE-BLOCK ;
\ First test below sets the scenario for the subsequent tests
\ BLK is unchanged by LIST
T{ BLK @ RND-TEST-BLOCK DUP TLS5 LIST BLK @ = -> TRUE }T
\ SCR is unchanged by Earlier words
T{ SCR @ FLUSH SCR @ = -> TRUE }T

: TUF-E T{ SCR @ FLUSH DUP 1+ BUFFER DROP SCR @ =
  -> TRUE }T ; TUF-E \ ." TUF-E "
: TUF-F T{ SCR @ FLUSH DUP 1+ BLOCK DROP SCR @ =
  -> TRUE }T ; TUF-F \ ." TUF-F "
: TUF-G T{ SCR @ FLUSH DUP 1+ BLOCK DROP UPDATE SCR @ =
  -> TRUE }T ; TUF-G \ ." TUF-G "
: TUF-H T{ SCR @ FLUSH DUP 1+ BLOCK
    DROP UPDATE SAVE-BUFFERS SCR @ = -> TRUE }T ;
TUF-H \ ." TUF-H "
: TLS6 ( blk -- )
    S" SCR @" WRITE-BLOCK ;
: TUF-I T{ SCR @ RND-TEST-BLOCK DUP TLS6 LOAD SCR @ OVER 2=
  -> TRUE }T ; TUF-I \ ." TUF-I "
TESTING EMPTY-BUFFERS%

T{ EMPTY-BUFFERS -> }T
T{ BLK @ EMPTY-BUFFERS BLK @ = -> TRUE }T
T{ SCR @ EMPTY-BUFFERS SCR @ = -> TRUE }T

\ Test R/W, but discarded changes with EMPTY-BUFFERS
: TUF-J T{ RND-TEST-BLOCK            \ blk
   DUP BLANK-BUFFER                  \ blk blk-addr1
   1024 ELF-HASH                     \ blk hash
   UPDATE FLUSH                      \ blk hash
   OVER BLOCK [CHAR] \ SWAP C!       \ blk hash
   UPDATE EMPTY-BUFFERS FLUSH        \ blk hash
   SWAP BLOCK                        \ hash blk-addr2
   1024 ELF-HASH = -> TRUE }T ;
TUF-J \ ." TUF-J "

\ EMPTY-BUFFERS discards all buffers
: TUF2-EB ( blk1 blk2 -- blk1 blk2 )
  TUF2-1 TUF2-2 EMPTY-BUFFERS ;  \ c.f. TUF2-3
: TUF-K T{ ['] TUF2-EB 2RND-TEST-BLOCKS TUF2
   2SWAP 2DROP 2= -> TRUE }T ;
TUF-K \ ." TUF-K "

\ FLUSH and then EMPTY-BUFFERS is acceptable but untestable
\ EMPTY-BUFFERS and then UPDATE is ambiguous and untestable

TESTING >IN manipulation from a block source%
: TIN ( blk -- )
  S" 1 8 >IN +!     2        3" WRITE-BLOCK ;
: TUF-L T{ RND-TEST-BLOCK DUP TIN LOAD -> 1 3 }T ;
TUF-L \ ." TUF-L "
\ -------------------------------------------------------------
TESTING \, from a block source%

64 CONSTANT C/L

: WRITE-BLOCK-LINE ( lin-addr[i] c-addr u -- lin-addr[i+1] )
    2>R DUP C/L CHARS + SWAP 2R> ROT SWAP CMOVE ;

\ Discards to the end of the line
: TCSIRIR1 ( blk -- )
  BLANK-BUFFER
  C/L 1024 U< IF
    S" 2222 \ 3333" WRITE-BLOCK-LINE
    S" 4444"        WRITE-BLOCK-LINE
  THEN
  DROP UPDATE SAVE-BUFFERS ;
: TUF-M T{ RND-TEST-BLOCK DUP TCSIRIR1 LOAD -> 2222 4444 }T ;
TUF-M \ ." TUF-M "

\ -------------------------------------------------------------
TESTING THRU%

: TT1 ( blks -- )
  DUP S" BLK" WRITE-BLOCK
  1+  S" @"  WRITE-BLOCK ;
: TUF-N T{ 2 RND-TEST-BLOCK-SEQ DUP TT1 DUP DUP 1+ THRU 1- =
  -> TRUE }T ;
TUF-N \ ." TUF-N "
\ -------------------------------------------------------------

\ FLA: What is this thing?
\ BLOCK-ERRORS SET-ERROR-COUNT

CR ." End of Block word tests"


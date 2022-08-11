\ Meta-II: a syntax directed compiler generator for ValgolI.
\ Original code by Demitri Peynado: May 13, 2021.

\ begin Z79Forth glue code.
: ABORT" POSTPONE IF
  POSTPONE ."
  POSTPONE ABORT
  POSTPONE THEN ; IMMEDIATE RESTRICT

: DEFER ( "name" -- )
  CREATE ['] ABORT ,
  DOES> ( ... -- ... )
    @ EXECUTE ;
: DEFER! ( xt1 xt2 -- ) 9 + ! ;         \ Z79Forth specific
: IS ( xt "<spaces>name" -- ) STATE @ IF
    POSTPONE [']   POSTPONE DEFER!
  ELSE
    FIND DEFER!
  THEN
; IMMEDIATE

\ This is extremely useful for debugging call stacks.
\ It may not work properly if bytes are on the system
\ stack of the 6309 though!
\ : sysstkdump S
\   BEGIN
\     DUP $8000 U<
\   WHILE  
\     CR DUP @ .'  
\     1 CELLS + 
\   REPEAT
\   DROP ;

\ :NONAME support. WARNING: because this is an application
\ level implementation, any error in a :NONAME definition
\ will cause the last named word to be forgotten!
\ Nested anonymous definitions are not supported.
VARIABLE isanon   0 isanon !
VARIABLE anonep         \ Execution token returned by :NONAME

: :NONAME HERE anonep !
  1 DUP STATE ! isanon ! ;

: ; isanon @ UNLESS     \ UNLESS is a shortcut for 0= IF
    ['] ; EXECUTE
    EXIT
  THEN
  0 DUP isanon ! STATE !
  $39 C,                \ Emit an RTS instruction
  anonep @              \ Return the execution token
; IMMEDIATE RESTRICT

VARIABLE curblk  \ Init'ed by reset-input, updated by advance
-1 CONSTANT true
0 CONSTANT false

\ end Z79Forth glue code.

CREATE tmp 50 ALLOT
VARIABLE input
VARIABLE copy
VARIABLE copy-len

\ Words to implement Meta II words
: reset-input curblk @ BLOCK input ! ;

: advance input @ 1+ C@ IF
    input 1+!
    EXIT
  THEN
  \ End of block detected. Switch to the next one.
  \ blocks 600-602 have the BNF syxtax description for the
  \ ValgolI language. Block 603 has a sample program.
  curblk @ 603 U> ABORT" Read beyond block 603!"
  curblk 1+!
  reset-input ;

: save input @ copy ! ;

: restore copy @ input ! ;

: end-save input @ copy @ - copy-len ! ;

: delete-blanks BEGIN
    input @ C@ 1 BL 1+ WITHIN
  WHILE
    advance
  REPEAT ;

: digit? ( -- ) input @ C@ [CHAR] 0 [CHAR] 9 1+ WITHIN ;

: letter? ( -- ) input @ C@ [CHAR] A [CHAR] z 1+ WITHIN ;

: @quote? ( -- ) @ C@ [CHAR] ' = ;

: quote? ( -- ) input @quote? ;

: letters/digits BEGIN
    advance letter? digit? OR NOT
  UNTIL ;

: match? ( addr count -- count flag ) true SWAP 0 DO
    OVER i + C@ input @ C@ advance = AND
  LOOP NIP ;

\ The following primitives are not meant to be invoked in
\ an arbitrary order. State is maintained through the updates
\ to 'copy' and 'copy-length'.
: copy>tmp copy @ tmp 2 + copy-len @ CMOVE tmp 2 + copy ! ;

: '>s" [CHAR] S copy @ 2 - C!
  [CHAR] " copy @ 1- C!
  BL copy @ C! ;

: '>" [CHAR] " copy @ copy-len @ 1- + C! ;

: ''len>s""len copy @ 2 - copy ! copy-len @ 2 + copy-len ! ;

: ''>s"" ( -- ) '>s" '>" ''len>s""len ;

\ Meta II words. All expect a flag to be on the stack
: bgn ( -- ) 0 CR ;

: set ( flag -- true ) DROP true ;

: rst ( flag -- false ) DROP false ;

: tst ( iflag str -- oflag ) delete-blanks save match? IF
    end-save set EXIT
  THEN
  restore rst ;

: num delete-blanks digit? IF
    set save
    BEGIN
      advance digit? NOT
    UNTIL
    end-save
    EXIT
  THEN
  rst ;

: id delete-blanks letter? IF
    set save letters/digits end-save EXIT
  THEN
  rst ;

\ Accept a character literal as '<char>+'
: sr delete-blanks quote? IF
    set save advance           \ copy points to the leading '
    BEGIN
      quote? NOT
    WHILE
      advance
    REPEAT
    advance end-save EXIT
  THEN
  rst ;

\ Some sort of assertion of what the input token should be.
: be DUP NOT ABORT" Syntax Error" ;

: cl ( str -- ) TYPE SPACE ;

: ci copy @quote? IF
    copy>tmp ''>s""
  THEN
  copy @ copy-len @ TYPE SPACE ;

: out CR ;

: end CR IF
    ." COMPILATION COMPLETE"
  ELSE
    ." ERRORS IN COMPILATION"
  THEN ;

\ Input
: bnfload 600 curblk !   \ Syntax spec. lives at blk 600-602
  reset-input ;

\ Meta II Meta Compiler Bootstrap
: out1 S" *" tst DUP IF 
    S" ci" cl out              \ Semantic action
  THEN 
  DUP NOT IF 
    sr dup IF
      ci S" cl" cl out         \ Semantic action
    THEN 
  THEN ;

: output S" .OUT" tst dup IF 
    S" (" tst be               \ Assertion
    BEGIN
      out1 DUP NOT
    UNTIL
    set be
    S" )" tst be               \ Assertion
    S" out" cl out             \ Semantic action
  THEN ;

DEFER ex1

: ex3 id DUP IF
    ci out
  THEN
  DUP NOT IF 
    sr DUP IF
      ci s" tst" cl out        \ Semantic action
    THEN
  THEN
  DUP NOT IF 
    S" .ID" tst DUP IF
      S" id" cl out            \ Semantic action
    THEN
  THEN
  DUP NOT IF 
    S" .NUMBER" tst DUP IF
      S" num" cl out           \ Semantic action
    THEN
  THEN
  DUP NOT IF 
    S" .STRING" tst DUP IF
      S" sr" cl out            \ Semantic action
    THEN
  THEN
  DUP NOT IF 
    S" (" tst DUP IF
      ex1 be                   \ Assertion
      s" )" tst be             \ Assertion
    THEN
  THEN
  DUP NOT IF 
    S" .EMPTY" tst DUP IF
      S" set" cl
    THEN
  THEN 
  DUP NOT IF 
    S" $" tst DUP IF
      S" BEGIN" cl out         \ Semantic action
      RECURSE
      S" DUP NOT UNTIL" cl out \ Semantic action
      S" set" cl out           \ Semantic action
    THEN
  THEN ;

: ex2 ex3 DUP IF 
    S" DUP IF" cl out          \ Semantic action
    BEGIN 
      ex3 DUP IF
        S" be" cl out          \ Semantic action
      THEN
      DUP NOT IF
        output
      THEN
      DUP NOT 
    UNTIL 
    set	
    S" THEN" cl out            \ Semantic action
  THEN 
  DUP NOT IF 
    output DUP IF 
      BEGIN 
        ex3 DUP IF
          S" be" cl out        \ Semantic action
        THEN
        DUP NOT IF
          output
        THEN
        DUP NOT
      UNTIL
      set
    THEN
  THEN ;

:NONAME
  ex2 BEGIN 
    S" /" tst DUP IF
      S" DUP NOT IF" cl out    \ Semantic action
      ex2 be                   \ Assertion
      S" THEN" cl out          \ Semantic action
    THEN
    DUP NOT 
  UNTIL
  set ; IS ex1

DEFER rule
:NONAME
  S" .RULE" tst DUP IF 
    id be S" DEFER" cl ci out  \ Semantic action
    BEGIN
      rule DUP NOT
    UNTIL
    set
    S" .BEGIN" tst be          \ Assertion
    S" :NONAME" cl out         \ Semantic action
    ex1 be                     \ Assertion
    S" .END" tst be            \ Assertion
    id be                      \ Assertion
    S" ; IS" cl ci out         \ Semantic action
  THEN ; IS rule

: program bgn
  S" .SYNTAX" tst DUP IF
    id be
    S" DEFER" cl ci out        \ Semantic action
    BEGIN
      rule DUP NOT
    UNTIL
    set
    S" .BEGIN" tst be          \ Assertion
    S" :NONAME" cl out         \ Semantic action
    ex1 be                     \ Assertion
    S" .END" tst be            \ Assertion
    id S" ; IS" cl ci out      \ Semantic action
  THEN
  end ;

\ Entry point here.
bnfload                        \ Load 1st block of syntax def.
program                        \ Generate compiler source code
\ The generated code has to be sourced from the console input.
603 LIST                       \ Sample ValgolI program
false valgoli DROP             \ Load the sample program
\ Cut and paste the output and invoke the sample program
CR run                         \ Run the sample program
dis run                        \ Showing off!


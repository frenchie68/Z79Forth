\ Tachyon Forth like "COMPEX CLI". FLA August 3, 2022.
\ Disclaimer: this is utterly implementation dependent stuff!
\ Disclaimer2: this shell is fine for interactive use,
\ however, when cut and pasting large multiple line code
\ chunks, characters may be lost, possibly resulting in
\ incorrect code generation.

\ :NONAME support. WARNING: because this is an application
\ level implementation, any error in a :NONAME definition
\ will cause the last named word to be forgotten!
\ Nested anonymous definitions are not supported.
VARIABLE isanon   0 isanon !
VARIABLE anonep            \ xt returned by :NONAME

: :NONAME HERE anonep !
  1 DUP STATE ! isanon ! ;

: ; isanon @ UNLESS        \ UNLESS is a shortcut for 0= IF
    ['] ; EXECUTE
    EXIT
  THEN
  0 DUP isanon ! STATE !
  $39 C,                   \ Emit an RTS instruction
  anonep @                 \ Return the execution token
; IMMEDIATE RESTRICT

\ Words that consume from the input stream in a stateless
\ manner or are otherwise unsuitable for anonynous compilation
\ are excluded and listed below.
\ " : :NONAME CONSTANT VARIABLE CREATE FIND ' FORGET CHAR SEE
\ DIS DEFER DEFINES END-CLASS METHOD VAR ;
\ Please note that S", .", ['], [CHAR], (, \ POSTPONE and IS
\ know how to handle themselves.
\ This code assumes that the disassembler has been previously
\ loaded from mass storage, i.e. that " is defined.
\ : " ( -- ) [CHAR] " WORD C@ 1+ ALLOT ;

CREATE nonanon-table
1 C, CHAR " C,             \ Can't use " to specify " itself
" :"
" :NONAME"
" CONSTANT"
" VARIABLE"
" CREATE"
" FIND"
" '"                       \ As a alias to FIND
" FORGET"
" CHAR
" DIS"
" SEE"                     \ As an alias to DIS
" DEFER"
" DEFINES"                 \ Used in SW/examples/bernd-oof.4th
" END-CLASS"               \ Used in SW/examples/bernd-oof.4th
" METHOD"                  \ Used in SW/examples/bernd-oof.4th
" VAR"                     \ Used in SW/examples/bernd-oof.4th
" ;"                       \ Can appear on a line of its own
0 C,                       \ End of table marker
MONITOR                    \ Mark this as read-only material

\ We need to tokenize the input line. The ouput is a vector
\ of starting token addresses for strings that are either BL
\ or NUL terminated.
#20 CONSTANT maxtokencount
VARIABLE tokencount
CREATE tokenvec   maxtokencount CELLS ALLOT

: tokenstart ( input -- updatedinput )
  1- BEGIN
    1+
    DUP C@ UNLESS          \ End of line detected
      EXIT
    THEN
    DUP C@ BL <>
  UNTIL                    \ Start of token detected
  tokencount @ maxtokencount = IF
    CR ." Too many tokens in the input stream" ABORT
  THEN
  \ Add token to vector
  DUP tokencount @ CELLS tokenvec + !
  tokencount 1+! ;

: tokenend ( input -- updatedinput )
  BEGIN
    1+ DUP C@ UNLESS       \ End of line detected
      EXIT
    THEN
    DUP C@ BL =
  UNTIL ;

\ This tokenizer is very crude yet it does the job
\ and leads to a very conservative approach in that
\ it might identify forbidden words in the middle of
\ string literals or comments--better safe than sorry!
: tokenize ( input -- )
  0 tokencount !
  BEGIN
    tokenstart
    DUP C@ UNLESS DROP EXIT THEN
    tokenend
  AGAIN ;

: toupper ( charin -- charout )
  DUP [CHAR] a [CHAR] z 1+ WITHIN IF
    32 -
  THEN ;

: ?endofword ( addr -- flag )
  C@ DUP 0= SWAP BL = OR ;

: ?match ( curtoka nonanon-table_entry -- flag )
  COUNT                    \ curtoka\entstr\entlen
  0 DO                     \ curtoka\entstr
    OVER ?endofword IF     \ End of curtoka detected
      UNLOOP 2DROP 0 EXIT
    THEN
    OVER C@ toupper
    OVER C@ <> IF
      UNLOOP 2DROP 0 EXIT
    THEN
    1+ SWAP 1+ SWAP
  LOOP
  \ Check for terminated curtoka
  DROP ?endofword ;

: ?isforbidden ( curtoka -- flag )
  >R                       \ R: curtoka
  nonanon-table >R BEGIN   \ R: curtoka\nonanon-table_entry
    R@ C@ UNLESS           \ End of nonanon-table reached
      UNLOOP 0 EXIT
    THEN

    \ Match the counted string at I with the token at I'
    I' I ?match IF
      UNLOOP 1 EXIT
    THEN

    R> DUP C@ + 1+ >R      \ Point to next nonanon-table_entry
  AGAIN ;

\ When the following is invoked, tokenvec is an array of
\ tokencount pointers corresponding to the input line being
\ under current consideration. We need to make sure that none
\ of these tokens are in nonanon-table of counted strings.
\ Input tokens characters are to be forced to uppercase
\ before actually performing the comparison.
: ?anonpossible ( -- flag )
  tokencount @ ?DUP UNLESS
    0 EXIT
  THEN
  0 DO                     \ Iterate over input line
    I CELLS tokenvec + @   \ curtoka to the data stack
    ?isforbidden IF
      UNLOOP 0 EXIT
    THEN
  LOOP
  1 ;                      \ No obstacle to anonymous comp.

CREATE intbuf
#120 ALLOT

\ Is anonynous compilation possible?
: ?anoncompile ( -- flag )
  STATE @ IF
    0 EXIT
  THEN
  \ We need to go over the token list for intbuf and find out
  \ (in a case insensitive manner) whether any token in the
  \ exclude list (nonanon-table) is present.
  intbuf tokenize
  ?anonpossible ;          \ Check for forbidden tokens

\ Expose some of the internal pointers. This is ugly but
\ necessary for an application level implementation.
$108 CONSTANT dicend       \ Writable HERE address
$10C CONSTANT bdicend      \ Writable backup address for HERE

: anoncompex ( -- )
  HERE bdicend !           \ Backup HERE
  HERE $1000 + dicend !    \ Point to compile scratch space
  :NONAME                  \ Switch to anonymous compilation
  >IN @ >R BLK @ >R        \ Backup >IN, BLK
  -1 BLK ! 0 >IN !
  INTERPRET
  ['] ; EXECUTE            \ End anonymous compilation
  bdicend @ dicend !       \ Restore HERE
  R> BLK ! R> >IN !        \ Restore BLK, >IN
  EXECUTE ;                \ Run the xt returned by :NONAME

\ Alternative interpreter entry point. On error, INTERP will
\ be aborted and control will resume to the native
\ interpreter. Dictionary integrity will be preserved though!
: INTERP ( -- ) CR
  BEGIN
    intbuf #120 EXPECT
    intbuf -1 BLOCK #120 CMOVE
    ?anoncompile IF
      anoncompex
    ELSE
      -1 LOAD
    THEN
    STATE @ UNLESS
      ."  :ok"
    THEN
    CR
  AGAIN ;

\ Typical invokation:
\ INTERP
\ #10 0 DO I . LOOP


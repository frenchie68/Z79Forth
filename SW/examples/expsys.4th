\ Z79Forth specific code by Francois Laagel: June 28, 2022.

\ Begin Z79Forth specific code.
\ We resort to -1 as a scratch BLOCK here so as to be able to
\ use FIND. Please note that BUFFER would not do it since
\ FIND insists on the buffer being mapped (read from CF).
: isvalidword ( -- flag )
  >IN @ >R BLK @ >R        \ Save >IN, BLK
  HERE COUNT 1+            \ nameaddr\1+namebcount
  -1 BLOCK SWAP CMOVE
  0 >IN ! -1 BLK ! FIND    \ FORTH-83/ANSI would have ' here
  R> BLK ! R> >IN ! ;      \ Restore BLK, >IN

: wordlen ( hdraddr -- wordlen ) C@ $1F AND ;

\ Extract the name field of the word whose header address is
\ passed as an argument. Add an extra space at the end
\ (uncounted). The output is to HERE.
: hdraddr>cstring ( flagsaddr -- cstringaddr cstringbcount )
  DUP wordlen              \ flagsaddr\wordlen
  DUP HERE C!              \ flagsaddr\wordlen
  SWAP 1+ SWAP             \ nameaddr\wordlen
  HERE 1+ SWAP CMOVE
  HERE COUNT               \ stringbaseaddr\stringbcount
  2DUP + BL SWAP C! ;      \ Add trailing space for FIND

\ Find out if RELFEAT was enabled in the running EEPROM image.
FIND ICHECK DROP PAYLOAD 1 <>
3 + CONSTANT minhdroverhead

\ WORDBYADDR (Forthwin): Retrieve word name as a counted
\ string that can be passed to TYPE. The word we lookup is
\ assumed to have been CREATEd, i.e. xxx returns FIND xxx 9 +.
\ Minimal header overhead is 4 bytes (3 if RELFEAT has been
\ disabled).
: wordbyaddr ( addr -- cstringaddr cstringbcount )
  \ Input validation
  DUP DUP 9 - DUP C@ $8E = \ $8E: opcode for LDX immediate
  SWAP 1+ @ ROT = AND UNLESS \ UNLESS is 0= IF
    HEX CR U. ." Not a CREATEd word" ABORT
  THEN

  9 - DUP >R
  minhdroverhead 1+ -      \ 1+ is for mimimum word name length
  BEGIN
    >R                     \ R: xt\<candidate flag/header addr>
    I wordlen              \ Extract possible word length
    I + minhdroverhead + I' = IF
      I hdraddr>cstring
      isvalidword IF
        R> R> 2DROP EXIT
      THEN
      2DROP
    THEN
    R> 1-
  AGAIN ;

: cell+ 1 CELLS + ;
: cell- 1 CELLS - ;
\ Redefine = and 0= for ANSI semantics (true is -1).
: = = IF -1 ELSE 0 THEN ;
: 0= 0 = ;
: 0<> 0= INVERT ;
\ End Z79Forth specific code.

: question: CREATE
  0 C,                     \ flag: question has been answered
  0 C, ;                   \ flag: the user's answer
: ans 1+ ;
: .question wordbyaddr TYPE SPACE ." (type y/n) " ;
: user-y/n KEY DUP EMIT CR [CHAR] y = ;
: ask ( q --  a ) 
  DUP C@ 0= IF
    DUP .question user-y/n OVER ans C! -1 OVER C!
  THEN 
  ans C@ 0<> ;

100 CONSTANT max_rules
CREATE rules max_rules CELLS ALLOT
\ nrules is the last registered rule's address in rules.
\ Ultimately nrules @ is zero.
rules VALUE nrules
: >rules nrules ! nrules cell+ TO nrules ;
: rule: CREATE HERE >rules ;
: ;rule , 0 , ;            \ Add rule and end of list marker
: prove ( rule -- ) -1 BEGIN
    OVER @ OVER AND
  WHILE
    OVER @ ask AND SWAP cell+ SWAP
  REPEAT NIP ;
: solve CR rules 0 
  BEGIN
    OVER @                 \ Acquire rule addr, 0 is last rule
    OVER 0=                \ Goal not proven yet
    AND
  WHILE 
    OVER @ prove OR SWAP cell+ SWAP 
  REPEAT
  CR IF
    ." Answer: " cell- @ wordbyaddr TYPE
  ELSE
    DROP ." Unknown"
  THEN ;

\ Database

question: slow-build-of-symptoms?
question: sore-throat?
question: runny-nose?
question: fever?
question: body-aches?
question: head-aches?
question: cough?
question: fatigue?
question: tiredness?
question: runny-nose?
question: no-loss-of-taste-or-smell?
question: congestion?
question: sore-throat?
question: cough?
question: immediate-onset-of-symptoms?
rule: covid-19(delta)
    slow-build-of-symptoms? ,
    head-aches? ,
    sore-throat? ,
    runny-nose? ,
    fever? ,
    body-aches?
;rule
rule: covid-19(omicron)
    slow-build-of-symptoms? ,
    cough? ,
    fatigue? ,
    tiredness? ,
    congestion? ,
    runny-nose? ,
    no-loss-of-taste-or-smell?
;rule
rule: common-cold
    slow-build-of-symptoms? ,
    runny-nose? ,
    congestion? ,
    cough? ,
    sore-throat?
;rule

\ Test patterns:
\ slow-build-of-symptoms? wordbyaddr CR TYPE CHAR * EMIT
\ slow-build-of-symptoms? ask
\ covid-19(delta) prove
\ Typical invocation:
\ solve


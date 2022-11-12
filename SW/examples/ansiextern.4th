\ ANSI Miscellaneous primitives for Z79Forth

\ Stolen from GNU Forth--literally!
\ This works, although I do not fully understand why or how!!!
: -TRAILING ( c-addr u1 -- c-addr u2 )
  BEGIN  DUP                  \ ANSI (String)
  WHILE  1- 2DUP + C@ BL <> 
  UNTIL 
  1+ 
  THEN ;

: (sgn) ( n -- -1/0/1 ) DUP 0= IF EXIT THEN  0< 2* 1+ ;

: bounds OVER + SWAP ;

: (-text) ( c_addr1 u c_addr2 -- n )
  SWAP bounds ?DO
    DUP C@ I C@ = WHILE
      1+
    LOOP
    DROP 0
  ELSE
    C@ I C@ - UNLOOP
  THEN  (sgn) ;

\ Compare the string specified by c-addr1 u1 to the string
\ specified by c-addr2 u2. The strings are compared, beginning
\ at the given addresses, character by character, up to the
\ length of the shorter string or until a difference is found.
\
\ - If the two strings are identical, n is zero.
\ - If the two strings are identical up to the length of the
\   shorter string, n is minus-one (-1) if u1 is less than u2
\   and one (1) otherwise.
\ - If the two strings are not identical up to the length of
\   the shorter string, n is minus-one (-1) if the first
\   non-matching character in the string specified by c-addr1
\   u1 has a lesser numeric value than the corresponding
\   character in the string specified by c-addr2 u2 and one (1)
\   otherwise.
: COMPARE ( c-addr1 u1 c-addr2 u2 -- n )
  ROT 2DUP SWAP - >R MIN SWAP (-text) DUP
  IF  R> DROP  ELSE  DROP r> (sgn)  THEN ;

: toupper  ( c1 -- c2 )
  DUP [CHAR] a - [ CHAR z CHAR a - 1+ ] LITERAL U< BL AND - ;

: ERASE ( addr u -- )
  0 FILL ;                    \ ANSI (Core ext)
: ENVIRONMENT? ( c-addr u -- false | i*x true )
  2DROP FALSE ;               \ ANSI (Core)

: UNUSED ( -- u )             \ ANSI (Core ext)
  $7C00 HERE - ;              \ 32 KB RAM, 1KB for sysstk

: ABORT"                      \ ANSI (Core)
  ( Compilation: "ccc<quote>" -- )
  ( Runtime: i*x x1 --  | i*x ) ( R: j*x --  | j*x )
  POSTPONE IF
    POSTPONE ."
    POSTPONE ABORT
  POSTPONE THEN ; IMMEDIATE RESTRICT

\ From forth-standard.org. ruv's 1st entry of 2021-05-21 22:33
: /STRING ( c-addr1 u1 n -- c-addr2 u2 )
  TUCK - >R CHARS + R> ;      \ ANSI (String)

\ From the DPANS94 standard specification.
: .( ( "ccc<paren>" -- )      \ ANSI (Core ext)
  [CHAR] ) WORD COUNT TYPE ; IMMEDIATE

\ -------------------------------------------------------------
\ ANSI >BODY for Z79Forth/A (implementation specific)

: >BODY ( xt -- a-addr )      \ ANSI (Core)
  \ Input validation
  DUP DUP C@ $8E =            \ $8E: opcode for LDX immediate
  OVER 1+ @ ROT 9 + = AND
  UNLESS                      \ UNLESS is 0= IF
    HEX CR U. ." Not a CREATEd word" ABORT
  THEN
  9 + ;

\ -------------------------------------------------------------
\ ANSI WITHIN
\ Source: forth-standard.org

: WITHIN                      \ ANSI (Core ext )
  ( n1|u1 n2|u2 n3|u3 -- flag )
  OVER - >R - R> U< ;

\ -------------------------------------------------------------
\ ANSI VALUE/TO for Z79Forth/A (implementation specific)

: VALUE                       \ ANSI (Core ext )
  ( Compilation: x "<spaces>name" -- )
  ( Execution: -- x )
  CONSTANT UNMONITOR ;

: TO                          \ ANSI (Core ext )
  ( Compilation: "<spaces>name" -- )
  ( Execution: x -- )
  >IN @                       \ symSrcStartAddress
  ' ?DUP  IF                  \ symSrcStartAddress\xt
    NIP  STATE @  IF          \ We are compiling
      POSTPONE LITERAL  POSTPONE 1+  POSTPONE !
    ELSE                      \ We are interpreting
      1+ !
    THEN
  ELSE                        \ symSrcStartAddress
    >IN !                     \ Symbol not found
  THEN ; IMMEDIATE

\ -------------------------------------------------------------
\ ANSI DEFER/IS for Z79Forth/A
\ Source: forth-standard.org

\ Forth 2012 (Core ext). Referred to as "common usage" in
\ Conklin/Rather's "Forth Programmer's Handbook."
: DEFER ( "name" -- )
  CREATE ['] ABORT ,
  DOES> ( ... -- ... )
    @ EXECUTE ;

: DEFER! ( xt1 xt2 -- ) >BODY ! ;

: DEFER@ ( xt1 -- xt2 ) >BODY @ ;

: IS ( xt "<spaces>name" -- ) STATE @ IF
    POSTPONE [']   POSTPONE DEFER!
  ELSE
    ' DEFER!
  THEN ; IMMEDIATE

: ACTION-OF
   STATE @ IF
     POSTPONE ['] POSTPONE DEFER@
   ELSE
     ' DEFER@
   THEN ; IMMEDIATE

\ -------------------------------------------------------------
\ ANSI LSHIFT/RSHIFT for Z79Forth (relies on 79-STANDARD SHIFT)

: LSHIFT ( x1 u -- x2 )       \ ANSI (Core)
  SHIFT ;
: RSHIFT ( x1 u -- x2 )       \ ANSI (Core)
  NEGATE SHIFT ;

\ -------------------------------------------------------------
\ ANSI Return stack manipulation primitives for Z79Forth

: 2R@ ( -- x1 x2 ) ( R:  x1 x2 -- x1 x2 )
  I' I ; RESTRICT             \ ANSI (Core ext)

: 2R> ( -- x1 x2 ) ( R:  x1 x2 -- )
  R> R> SWAP ;  RESTRICT      \ ANSI (Core ext)

: 2>R ( x1 x2 -- ) ( R:  -- x1 x2 )
  SWAP >R >R ;  RESTRICT      \ ANSI (Core ext)


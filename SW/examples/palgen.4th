\ Palindrome generation. The trick is not to think of the
\ problem in terms of filtering but as a pattern generator.

: .reverse ( addr bcount -- ) DUP 0= IF 2DROP EXIT THEN
  1- SWAP 1+ DUP C@ \ bcount\addr\char                          
  -ROT ( char\bcount\addr ) SWAP   RECURSE EMIT ;
: num>str ( d -- addr bytecount ) <# #S #> ;
: .palindromes  ( -- ) CR 1000 0 DO
    I 10 MOD IF
      I 0 num>str   2DUP
      .reverse   TYPE   SPACE
    THEN
  LOOP ;

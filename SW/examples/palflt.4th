MARKER pal.marker
: num>str ( n -- addr bytecount ) S>D <# #S #> ;
: lasteqfirst? ( addr offsetlast -- flag )
  OVER + C@ SWAP C@ = ;

: ispalindrome? ( addr offsetlast -- flag )
  DUP 1 <              IF 2DROP 1 EXIT THEN
  2DUP lasteqfirst? 0= IF 2DROP 0 EXIT THEN
  2 - SWAP 1+ SWAP RECURSE ;

: pal ( -- ) CR 10 BEGIN
    DUP   num>str  \ n\addr\bcount
    2DUP 1-        \ n\addr\bcount\addr\bcount-1
    ispalindrome?  \ n\addr\bcount\flag
    IF DUP 7 SWAP - SPACES \ format output to 7 chars
      TYPE SPACE
    ELSE 2DROP THEN
    1+ DUP 50000 =
  UNTIL DROP ;            pal pal.marker

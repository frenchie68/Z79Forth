CREATE dump.marker

\ Header code contributed by Paul E. Bennett.
: .doubles ( -- ) $10 0 DO
    [CHAR] 0 EMIT I 1 .R SPACE
  LOOP ;
: .singles ( -- ) $10 0 DO
    I 1 .R
  LOOP ;
: .header ( -- ) 1 CELLS 2* 1+ SPACES .DOUBLES .SINGLES CR ;

: .addr S>D <# 1 CELLS 2* 0 DO # LOOP #> TYPE SPACE ;
: .data S>D <# # # #> TYPE SPACE ;
: tochar DUP BL $7F WITHIN UNLESS DROP [CHAR] . THEN ;

VARIABLE start            \ Base addr. of the interesting area
VARIABLE bytecount        \ Number of interesting bytes
VARIABLE asciib $E ALLOT  \ Buffer for the ASCII character dump

: dump.initvars.prtheader ( baseaddr bytecount -- align16 )
  bytecount ! DUP start ! $FFF0 AND
  ( baseaddr bytecount ) HEX CR .header ;

: asciidump asciib $10 TYPE ;

: dump.sanitycheck ( baseaddr bytecount -- baseaddr bytecount )
    DEPTH 2 < IF ABORT THEN
  DUP 0 <= IF ABORT THEN ;

: dump.iskip.bcount ( addr -- skipbytecount )
  start @ $FFF0 AND = IF
    start @ $F AND EXIT
  THEN 0 ;

: dump ( baseaddr bypecount -- ) dump.sanitycheck
  BASE @ >R
  dump.initvars.prtheader >R
  BEGIN                   \ R: saved_BASE currentdumpaddr
    I $F AND UNLESS       \ Beginning of a dump line
      I .addr             \ Print the base block address
      asciib $10 BL FILL  \ Initialize the ASCI dump area
    THEN

    I dump.iskip.bcount ?DUP IF
      \ Interesting data not reached yet
      DUP 3 * SPACES
      R> + >R             \ Skip the uninteresting part
    THEN

    I C@ DUP .data
    tochar I $F AND asciib + C!
    bytecount DUP @ 1- OVER !  \ bytecount

    @ UNLESS              \ Last dump line
      $F I $F AND - 3 * SPACES
      asciidump SPACE
      R> DROP R> BASE !
      EXIT                \ We are done here.
    THEN

    R> 1+ >R
    I $F AND UNLESS       \ Last column of non-last line
      asciidump CR
    THEN
  AGAIN ;

\ ' DOES> PAYLOAD dump


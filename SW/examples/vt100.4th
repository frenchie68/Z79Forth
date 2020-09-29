###############################################################
( Input keycodes )
( Left arrow	ESC [ D		1B 5B 44    )
( Right arrow	ESC [ C		1B 5B 43    )
( Up arrow	ESC [ A		1B 5B 41    )
( Down arrow	ESC [ B		1B 5B 42    )
( Home		ESC [ 1 ~	1B 5B 31 7E )
( End		ESC O F		1B 4F 46    )

###############################################################
( Begin Things to be removed eventually )

: .H BASE @ SWAP HEX . BASE ! ;

( Device Status Report ) CHAR n E.VT100-1PCMD E.VT100.DSR

VARIABLE E.PADPTR        VARIABLE E.POLLCOUNT

: E.GETKEYCODES ( -- )
  0 E.POLLCOUNT ! PAD E.PADPTR ! BEGIN
    KEY? IF
      KEY E.PADPTR @ C!
      E.PADPTR 1+!
    ELSE
      E.POLLCOUNT 1+!
      E.POLLCOUNT @ 40 > IF EXIT THEN
    THEN
    1 MS
  AGAIN ;

: E.EXTRACT.PARAM ( addr1 delim limit -- val1 addr2 )
  >R >R 0 SWAP BEGIN ( D: val1 addr2, R: limit delim )
    DUP I' U< UNLESS R> DROP R> DROP DROP 0 EXIT THEN
    DUP C@ DUP I = IF DROP 1+ E.2RDROP EXIT THEN ( Delim match )
    ( D: val1 addr2 char ) E.ASC.0 - ROT 10 * + SWAP 1+
  AGAIN ;

( Retrieve actual col/lin parameters in VT100 address space, )
( i.e. starting at 1,1. This is meant to be used in E.SITREP )
( to make sure we are in sync with the terminal emulator. )
: E.VT100.CUP.GETXY ( -- vtcol# vtlin# )
  6 E.VT100.DSR E.GETKEYCODES
  E.PADPTR @ PAD - 6 < IF 0 DUP EXIT THEN
  PAD 2+ [CHAR] ; E.PADPTR @ E.EXTRACT.PARAM
  OVER UNLESS DROP 0 DUP EXIT THEN ( Unmatched delimiter )
  ( D: vtlin# addr ) [CHAR] R E.PADPTR @ E.EXTRACT.PARAM
  DROP SWAP ;
( End Things to be removed eventually )

###############################################################

$a CONSTANT rtcrega      $b CONSTANT rtcregb 
: string-table CREATE ( nelts -- ) ,
  DOES> ( array-offset1 -- ) >R \ array-offset1 R: selfaddr
  DUP 1 R@ @ 1+ WITHIN UNLESS
    R> 2DROP ." ??? " EXIT     \ Offset is out of bounds
  THEN
  \ array-offset1 R: selfaddr
  R> 1 CELLS + SWAP 1- 
  ?DUP IF
    0 DO \ First counted string address, R:
      DUP C@ + 1+
    LOOP
  THEN COUNT TYPE SPACE ;

12 string-table month-table
" Jan"   " Feb"   " Mar"   " Apr"   " May"   " Jun"
" Jul"   " Aug"   " Sep"   " Oct"   " Nov"   " Dec"
MONITOR

7 string-table dow-table
" Sun"   " Mon"   " Tue"   " Wed"   " Thu"   " Fri"   " Sat"
MONITOR

: seconds 0 RTC@ ;    : minutes 2 RTC@ ;    : hours 4 RTC@ ;
: dow 6 RTC@ ;        : dom 7 RTC@ ;        : month 8 RTC@ ;
: year 9 RTC@ ;

: uipclearwait ( -- ) rtcrega RTC@ $80 AND IF 4 MS THEN ;

: rttimeset ( hours minutes seconds -- ) uipclearwait
  rtcregb RTC@ >R
  R@ $80 OR rtcregb RTC!       \ set the SET bit in RTCB
  0 RTC! 2 RTC! 4 RTC!         \ set seconds minutes and hours
  R> $7F AND rtcregb RTC! ;    \ clear the SET bit in RTCB

\ From Alain Pinaud, "Programmer en Forth" Edt. by P.S.I., 1983
: mkdow ( day month year -- dow )
  OVER 2 >
  IF SWAP 1+ ELSE 1- SWAP 13 + THEN
  SWAP DUP 4 / DUP 25 / DUP 4 /
  ROT + SWAP - + SWAP 306 * 10 / +
  + 6 + 7 MOD 1+ ;

: rtdateset ( month day year -- ) uipclearwait
  rtcregb RTC@ >R
  R@ $80 OR rtcregb RTC!       \ set the SET bit in RTCB
  >R >R >R
  J 9 RTC!                     \ set year
  I 8 RTC!                     \ set month
  I' 7 RTC!                    \ set day
  R> R> R> ROT SWAP 2000 +     \ day\month\year
  mkdow 6 RTC!                 \ set dow
  R> $7F AND rtcregb RTC! ;    \ clear the SET bit in RTCB

: 2digitsout ( n -- ) S>D <# # # #> TYPE ;

: date ( -- ) CR uipclearwait
  year seconds minutes hours month dom dow
  dow-table . month-table
  2digitsout [CHAR] : EMIT
  2digitsout [CHAR] : EMIT
  2digitsout ."  CET 20" 2digitsout ;

MARKER rtc.marker
: rtcinitialized? ( -- )
  rtcregb RTC@ $80 AND IF \ the SET bit is NZ in RTCB
    ." Please setup the RTC calendar using rttimeset"
    ."  and rtdateset"
  THEN ;
rtcinitialized? rtc.marker

\ -------------------------------------------------------------
: d. <# #S #> TYPE ;

: tickstest TICKS BEGIN
    TICKS 2DUP CR
    d. SPACE 2DUP >R >R
    2OVER DNEGATE D+ DROP [CHAR] [ EMIT 2 U.R [CHAR] ] EMIT
    2DROP R> R>
    1000 MS
  AGAIN ;

\ -------------------------------------------------------------
: truc CR
  BEGIN TICKS DROP $3FF AND $3FF =
    IF DATE 20 MS THEN
  AGAIN ;


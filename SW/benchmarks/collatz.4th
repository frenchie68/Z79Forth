( Collatz-Schritt. Null wenn Ueberlauf )
: cn+1                 ( cn -- cm )
  2 /mod swap                        
  if dup 10922 <       ( kein ueberlauf ? )
    if 3 * 2 +
    else drop 0 then
  then
;
( Collatz-Folge drucken                         )
( dieses Wort wird im Benchmark nicht benoetigt )
( es druckt die Collatz-Folge aus               )
: coll.                ( cn -- )
  cr
  begin dup 1 > while
    cn+1 dup 8 u.r
  repeat
  drop                 ( immer 1 )
;
( Collatz-Folge zaehlen. Null wenn Ueberlauf )
: ccnt                 ( cn -- cnt)
  0 swap               ( cnt cn )
  begin dup 1 > while
  cn+1 dup 
        if swap 1+ swap ( zaehlen )
        else drop 0
        then
  repeat
  drop
;
( Maximum fuer alle Folgen bis k bestimmen )
: cmax                 ( k -- max )
  0 swap               ( max k )
  begin dup 0 > while
    dup ccnt           ( max k cnt )
    rot                ( k cnt max )
    max                ( k max )
    swap               ( max k )
    1-                 ( max k-1 )
  repeat
  drop
;
( Funktionstest, Ergebniss muss die Zahl "126" sein )
32101 cmax .

\ 79-STANDARD @ 3 MHz native: 5m51s
\ 79-STANDARD @ 4 MHz native: 4m23s
\ 79-STANDARD @ 5 MHz native: 3m30s
\ ANS94 @ 4 MHz native: 4m52s
 

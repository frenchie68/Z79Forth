\ Pablo Hugo Reda's code for palindrome numbers. May 29, 2021
MARKER pal.marker
: testp ( n -- f )     \ Output is in negative logic
  0 OVER BEGIN         \ n f n
    10 /MOD SWAP       \ n f n/10 n%10
    ROT                \ n n/10 n%10 f
    10 * +             \ n n/10 10*f+n%10
    SWAP DUP           \ n 10*f+n%10 n/10 n/10
  WHILE REPEAT DROP - ;

: main ( -- ) CR
  0 BEGIN
    DUP 32767 = IF DROP EXIT THEN
    DUP testp 0= IF DUP 7 .R SPACE THEN 1+
  AGAIN ;
main pal.marker

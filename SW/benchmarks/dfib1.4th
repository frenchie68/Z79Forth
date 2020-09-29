DECIMAL
: d. ( d -- ) <# # #S #> TYPE ;
: dfib1 ( d1 -- d2 )
  2DUP 2 0 D< IF
    2DROP 1 0
  ELSE
    2DUP
    -1 -1 D+ RECURSE
    2SWAP -2 -1 D+ RECURSE
    D+
  THEN ;

\ -------------------------------------------------------------
24 0 dfib1 d.
\ 75025 is displayed, i.e. fib(25)
\ @ 3 MHz native: 52s
\ @ 4 MHz native: 39s
\ @ 5 MHz native: 31.5s

29 0 dfib1 d.
\ 832040 is displayed, i.e. fib(30)
\ @ 3 MHz native: 9m37s
\ @ 4 MHz native: 7m12s
\ @ 5 MHz native: 5m48s

34 0 dfib1 d.
\ 9227465 is displayed, i.e. fib(35)
\ @ 3 MHz native: 1h46m25s
\ @ 4 MHz native: 1h19m49s
\ @ 5 MHz native: 1h03m51s

\ Fact checking source: https://oeis.org/A000045/b000045.txt
\ -------------------------------------------------------------


: BENCHME ( xt n -- )
  DUP >R 0 DO
    DUP EXECUTE
    [CHAR] . EMIT
  LOOP
  DROP R> SPACE . ." Iterations." SPACE ;

\ -------------------------------------------------------------

HEX 
5 CONSTANT FIVE 
VARIABLE BVAR 

: BENCH 
  100 0 DO 
    1 BEGIN 
        DUP SWAP 
        DUP ROT DROP 
        1 AND IF 
          FIVE + 
        ELSE 
          1- 
        THEN 
        BVAR ! BVAR @ 
        DUP 0100 AND 
     UNTIL DROP 
   LOOP ;

DECIMAL
FIND BENCH 50 BENCHME

\ @ 3 Mhz native: 7m53s for 50 rounds--9.5s per round
\ @ 4 Mhz native: 5m55s for 50 rounds--7.1s per round
\ @ 5 Mhz native: 4m44s for 50 rounds--5.7s per round


\ Insert one blank block in front of a given a block interval.

DECIMAL

: BLKINS ( lowblkno highblkno -- ) 2DUP U< UNLESS
    2DROP ." usage: lowblkno highblkno BLKINS" EXIT
  THEN
  OVER >R           \ Save the lower bound to the return stack
  DO
    I BLOCK         \ ibufaddr
    I 1+ BUFFER     \ ibufaddr\i+1bufaddr
    1024 CMOVE  UPDATE
  -1 +LOOP
  R>                \ Restore the lower bound block number
  BUFFER 1024 BLANKS UPDATE FLUSH ;


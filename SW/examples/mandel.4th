\ Mandelbrot generation, ASCII style
\ Derived from https://github.com/Martin-H1/Forth-CS-101

\ Setup constants to remove magic numbers to allow
\ for greater zoom with different scale factors.
20 CONSTANT MAXITER
-39 CONSTANT MINVAL
40 CONSTANT MAXVAL

: lshift SHIFT ;

20 5 lshift CONSTANT RESCALE
RESCALE 4 * CONSTANT S_ESCAPE

\ These variables hold values during the escape calculation.
VARIABLE CREAL
VARIABLE CIMAG
VARIABLE ZREAL
VARIABLE ZIMAG
VARIABLE COUNTER

\ Compute squares, but rescale to remove extra scaling factor.
: ZR_SQ ZREAL @ DUP RESCALE */ ;
: ZI_SQ ZIMAG @ DUP RESCALE */ ;

\ Translate escape count to ascii greyscale.
: .CHAR S" ..,'~!^:;[/<&?oxOX#   " DROP + C@ EMIT ;

\ Numbers above 4 will always escape,
\ so compare to a scaled value.
: ESCAPES? S_ESCAPE > ;

\ Increment count and compare to max iterations.
: COUNT_AND_TEST?
  COUNTER @ 1+ DUP COUNTER !
  MAXITER > ;

\ Stores the row column values from the stack for the
\ escape calculation.
: INIT_VARS
  5 lshift DUP CREAL ! ZREAL !
  5 lshift DUP CIMAG ! ZIMAG !
  1 COUNTER ! ;

\ Performs a single iteration of the escape calculation.
: DOESCAPE ZR_SQ ZI_SQ 2DUP +
  ESCAPES? IF
    2DROP -1    \ Return the ANSI true
  ELSE
    - CREAL @ + \ Leave result on stack
    ZREAL @ ZIMAG @ RESCALE */ 2*
    CIMAG @ + ZIMAG !
    ZREAL ! \ Store stack item into ZREAL
    COUNT_AND_TEST?
  THEN ;

\ Iterates on a single cell to compute its escape factor.
: DOCELL INIT_VARS
  BEGIN
    DOESCAPE
  UNTIL
  COUNTER @ .CHAR ;

\ For each cell in a row.
: DOROW MAXVAL MINVAL DO
    DUP I
    DOCELL
  LOOP DROP ;

\ For each row in the set.
: MANDELBROT CR
  MAXVAL MINVAL DO
    I DOROW CR
  LOOP ;

\ Run the computation.
MANDELBROT


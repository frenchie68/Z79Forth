\ Game of Life - PEB
\ In memory of the late John Horton Conway, inventor of the
\ first computerised version in 1970. John Horton Conway died
\ from the Corona Virus (COVID-19) pandemic on 11th April 2020.
\
\ Game board is a square matrix of bit-cells that is cellwidth
\ wide and cellwidth cells high. This means it will be 16 bits
\ by 16 words on a 16 bit machine and 32 bits by 32 cells on a
\ 32 bit machine.
\
\ Each displayed bit cell is one ASCII character space on a
\ normal text screen. The program is written for VFX-Linux
\ version but may run on other standard Forths with some minor
\ shimming additions (eg Random being implemented on GForth).
\
\ *************************************************************
\ RULES: -
\
\ 1. Any live cell with two or three live neighbors survives.
\ 2. Any dead cell with three live neighbors becomes a live
\    cell.
\ 3. All other live cells die in the next generation.
\    Similarly, all other dead cells stay dead.
\ ************************************************************* 

\ VOCABULARY Life ONLY FORTH ALSO Life Life DEFINITIONS

DECIMAL
: .Title ( -- )
\ *G Print a title bar
  CR ."                        The Game of Life"
  CR ." In memory of John Horton Conway 26 Dec 1937 - "
  ." 11 Apr 2020" ;

\ *************************************************************
\ Glue Code
\ *************************************************************
: OFF 0 SWAP ! ;
: 0<> IF -1 ELSE 0 THEN ;
\ Add ANSI flag semantics to the builtin WITHIN
: WITHIN WITHIN 0<> ;
: 2R@ I' I ;
: 2R> R> R> SWAP ;
: 2>R SWAP >R >R ;
\ Test code for the above double primitives.
\ : test 1 2 2>R   2R@ .S 2DROP CR   2R> .S 2DROP ;

\ *************************************************************
\ Variables and Constants
\ *************************************************************
VARIABLE seed  23741 seed !
16 CONSTANT cellwidth \ width in bits of a single cell

VARIABLE adjacents
VARIABLE Generation
CREATE board cellwidth CELLS ALLOT     \ allocate board space
CREATE new-board cellwidth CELLS ALLOT \ allocate nextgen space

\ -------------------------------------------------------------
\ Basic Logic Functions
\ -------------------------------------------------------------
: inrange? ( n -- n\flag )
\ *G Set flag based on whether or not the number n is within
\ ** the permitted range of values.
  DUP
  0 cellwidth
  WITHIN
;
\ -------------------------------------------------------------

: 2^n ( n -- 2^n )
\ *G A means to generate a single bit that is 2 to the power of
\ ** the index n. n is checked for being no greater than the
\ ** maximum cell width. Input range is checked and out of range
\ ** inputs return a zero.
  inrange? IF
    1 SWAP SHIFT
  ELSE
    DROP 0
  THEN
;
\ -------------------------------------------------------------

\ *************************************************************
\ Board Layout
\ *************************************************************

: .Hatch ( -- )
\ *G Display a hatched pattern to the screen
   [CHAR] * EMIT
;
\ -------------------------------------------------------------

: .Blank ( -- )
\ *G Display a blank space to the screen.
  SPACE
;
\ -------------------------------------------------------------

\ *************************************************************
\ Board Initialisation, Management and Display
\ *************************************************************

: Row@ ( row -- data|0 )
\ *G If the row number is in range, grab the data from the
\ ** board array. Return a value of zero if out of range and
\ ** the data itself if in range.
  inrange? DUP >R
  AND CELLS board +
  @ R> AND
;
\ -------------------------------------------------------------

: Row! ( data\row -- )
\ *G Stow data in the designated row to replace the row data
\ ** in the board array.
  inrange?
  IF   CELLS board + !
  ELSE 2DROP
  THEN
;
\ -------------------------------------------------------------

: NGRow@ ( row -- data|0 )
\ *G If the row number is in range, grab the data from the
\ ** board array. Return a value of zero if out of range and
\ ** the data itself if in range.
  inrange? DUP >R
  AND CELLS new-board +
  @ R> AND
;
\ -------------------------------------------------------------

: NGRow! ( data\row -- )
\ *G Stow data in the designated row to replace the row data in
\ ** the board array.
  inrange?
  IF    CELLS new-board + !
  ELSE 2DROP
  THEN
;
\ -------------------------------------------------------------

: .cell ( mask\n -- )
\ *G Process the printing of cell state based on the bit
\ ** number. Decrement bit number.
  2^n AND
  IF   .hatch
  ELSE .blank
  THEN
;
\ -------------------------------------------------------------

: .bline  ( row -- )
\ *G Print hatch or space dependent on the state of each of the
\ ** bits in the extracted board line denoted by row. The order
\ ** of printing on the screen is highest order bit to the
\ ** left.
  >R CR R@ 3 .r 2 SPACES        \ --            (R row)
  board R> CELLS + @            \ data          (R --)
  0 cellwidth 1-                \ data\low\high
  DO   DUP I .cell -1           \ data
  +LOOP                         \ data
  DROP
;
\ -------------------------------------------------------------

: .Nbline  ( row -- )
\ *G Print hatch or space dependent on the state of each of the
\ ** bits in the extracted board line denoted by row. The order
\ ** of printing on the screen is highest order bit to the
\ ** left.
  >R CR R@ 3 .r 2 SPACES        \ --            (R row)
  new-board r> CELLS + @        \ data          (R --)
  0 cellwidth 1-                \ data\low\high
  DO   DUP I .cell -1           \ data
  +LOOP                         \ data
  DROP
;
\ -------------------------------------------------------------

: new>old ( -- )
  new-board board cellwidth MOVE
;
\ -------------------------------------------------------------

: .board ( -- )
\ *G Print the Game of Life current board state and the current
\ ** generation number.
  PAGE .title CR
  cellwidth 0
  DO   I .bline
  LOOP
  3 SPACES ." Generation = "
  Generation @ U.
;
\ -------------------------------------------------------------

: .Nboard ( -- )
\ *G Print the Game of Life New board state.
  cellwidth 0
  DO   I .Nbline
  LOOP
;
\ -------------------------------------------------------------

: zero ( caddr -- )
\ *G Fill the board with all zeroes
  cellwidth CELLS 0 FILL
;
\ -------------------------------------------------------------

: mark ( caddr -- )
\ *G Fill the board with all ones.
  cellwidth CELLS 255 FILL
;
\ -------------------------------------------------------------

: random ( -- N )  seed @ 613 * 5179 + DUP seed ! ;

: Random-fill ( -- )
\ *G Fill the whole board with random values.
  cellwidth 0
  DO   random 
       board I CELLS + !
  LOOP
;
\ -------------------------------------------------------------

\ *************************************************************
\ Rule Checking Logic
\ *************************************************************

: Y+? ( row\bit -- flag )
\ *G Check bit-state one row up board
  >R 1- row@ R> 2^n AND 0<>
;
\ -------------------------------------------------------------

: Y-? ( row\bit -- flag )
\ *G Check bit-state one row down board
  >R 1+ row@ R> 2^n AND 0<>
;
\ -------------------------------------------------------------

: X-? ( row\bit -- flag )
\ *G Check bit-state one column right on board
  >R row@ R> 1+ 2^n AND 0<>
;
\ -------------------------------------------------------------
  
: X+? ( row\bit -- flag )
\ *G Check bit-state one column left on board
  >R row@ R> 1- 2^n AND 0<>
;
\ -------------------------------------------------------------

: UL? ( row\bit -- flag )
\ *G Check bit-state one row up and one column left on board
  >R 1- row@ R> 1- 2^n AND 0<>
;
\ -------------------------------------------------------------

: DL? ( row\bit -- flag )
\ *G Check bit-state one row down and one column left on board
  >R 1+ row@ R> 1+ 2^n AND 0<>
;
\ -------------------------------------------------------------
 
: UR? ( row\bit -- flag )
\ *G Check bit-state one row down and one column left on board
  >R 1- row@ R> 1+ 2^n AND 0<>
;
\ -------------------------------------------------------------

: DR? ( row\bit -- flag )
\ *G Check bit-state one row down and one column right on board
  >R 1+ row@ R> 1- 2^n AND 0<>
;
\ -------------------------------------------------------------

: X0? ( row\bit -- flag )
\ *G Check bit-state one column right on board
  >R row@ R> 2^n AND 0<>
;
\ -------------------------------------------------------------

: Adjacents? ( -- flag )
\ *G Check the variable Adjacents for values in the range of
\ ** 2 or 3. flag is true if within the range and false for all
\ ** other values.
  adjacents @ 2 4 WITHIN
;
\ -------------------------------------------------------------

: Adjacents3=? ( -- flag )
\ *G flag is true if value of the Adjacents variable =3. flag
\ ** is false for all other values.
  adjacents @ 3 = 0<>
;

: nkill ( row\bit -- )
\ *G Grab a row from the new board and reset the bit indicated
\ ** by bit. If not modified by NBirth or NLives, then this
\ ** value is the one reposted.
  >R DUP R> 2^n INVERT  \ row\row\mask
  SWAP ngrow@           \ row\mask\data
  AND                   \ row\'data
  SWAP ngrow!
;
\ -------------------------------------------------------------

: nlive? ( row\bit -- )
  2DUP X0?              \ row\bit\flag1
  IF   Adjacents?       \ row\bit\flag2
  ELSE Adjacents3=?     \ row\bit\flag3
  THEN  -ROT            \ flag\row\bit
  2^n                   \ flag\row\mask
  ROT AND               \ row\'mask
  OVER ngrow@           \ row\'mask\data
  OR SWAP               \ 'data\row
  ngrow!
;
\ -------------------------------------------------------------

: cell-rule ( row\bit -- )
\ *G Apply the Cell Rules with the eight adjacent cells that
\ ** encircle the referenced cell. Sets or resets bits in the
\ ** coresponding New-Board space.
  adjacents OFF 2>R
  2R@ x+? ABS adjacents +!
  2R@ ur? ABS adjacents +!
  2R@ y+? ABS adjacents +!
  2R@ ul? ABS adjacents +!
  2R@ x-? ABS adjacents +!
  2R@ dl? ABS adjacents +!
  2R@ y-? ABS adjacents +!
  2R@ dr? ABS adjacents +!
  2R@ nkill
  2R> nlive?
;
\ -------------------------------------------------------------

: (WithRules) ( row -- )
\ *G Process a complete row
  0 cellwidth 1-
  DO   DUP I
       cell-rule
       -1
  +LOOP DROP
;
\ -------------------------------------------------------------

: WithRules ( -- )
\ *G Process all rows of the game board.
  cellwidth 0
  DO   I (withrules)
  LOOP
;
\ -------------------------------------------------------------

\ *************************************************************
\ Game Play
\ *************************************************************

: (Play) ( -- )
\ *G Play the Game of Life from the current position and
\ ** continue until any key is pressed.
  BEGIN PAGE
        .board                                         \ 100 MS
        WithRules
        1 Generation +!
        new>old
        KEY?
  UNTIL KEY DROP
;
\ *************************************************************

: Life ( -- )
\ *G Run the Game of Life. All starts to the game are begun
\ ** from the point of the last random fill and display of
\ ** the game board.
  generation OFF
  board mark .board 1000 MS
  board zero .board 1000 MS
  new-board zero
  random-fill
  (Play)
  generation OFF
  Adjacents OFF
;

\ *************************************************************
\ Starting points
\ *************************************************************

Life


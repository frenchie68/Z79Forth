\ Mini object oriented Forth. Bernd Paysan. Sept 24, 1998.
\ Ref: https://bernd-paysan.de/mini-oof.html

\ ------------------------------------------------------------
\ begin Z79Forth glue code.

: /string DUP >R - SWAP R> + SWAP ;
: cell 1 CELLS ;

\ end Z79Forth glue code.

\ ------------------------------------------------------------
\ begin Bernd's mini OOF kernel code.

: method ( m v -- m' v )
  CREATE
    OVER , SWAP CELL+ SWAP
  DOES> ( ... o -- ... )
    @ OVER @ + @ EXECUTE ;

: var ( m v size -- m v' )
  CREATE
    OVER , +
  DOES> ( o -- addr )
    @ + ;

CREATE object cell , 2 CELLS ,
: class ( class -- class methods vars ) DUP 2@ ;

: noop ;

: end-class  ( class methods vars -- )
  CREATE HERE >R , DUP , 2 CELLS
  2DUP = IF                \ ?DO is nice but unsupported
    2DROP
  ELSE
    DO
      ['] noop , cell
    +LOOP
  THEN
  CELL+ DUP CELL+ R> ROT @
  2 CELLS /string move ;

: defines ( xt class -- ) ' >body @ + ! ;

: new ( class -- o ) HERE OVER @ ALLOT SWAP OVER ! ;

: :: ( class "name" -- ) ' >body @ + @ COMPILE, ;

\ end Bernd's mini OOF kernel code.

\ ------------------------------------------------------------
\ begin Bernd's application example.

object class
  cell var text
  cell var len
  cell var x
  cell var y
  method init
  method draw
end-class button

\ begin imported material from GNU Forth vt100.fs:
: pn    BASE @ SWAP DECIMAL 0 U.R BASE ! ;
: ;pn   [CHAR] ; EMIT pn ;
: ESC[  #27 EMIT [CHAR] [ EMIT ;

: at-xy ( u1 u2 -- ) \ facility at-xy
  \ Position the cursor so that subsequent text output will
  \ take place at column @var{u1}, row @var{u2} of the
  \ display. (column 0, row 0 is the top left-hand corner of
  \ the display).
  1+ SWAP 1+ SWAP ESC[ pn ;pn [CHAR] H EMIT ;
\ end imported material from GNU Forth vt100.fs:

:noname ( o -- ) >R
  R@ x @ R@ y @ at-xy R@ text @ R> len @ TYPE ;
  button defines draw

:noname ( addr u o -- ) >R
  0 R@ x ! 0 R@ y ! R@ len ! r> text ! ;
  button defines init

button class
end-class bold-button

: bold   #27 emit ." [1m" ;
: normal #27 emit ." [0m" ;

:noname bold [ button :: draw ] normal ;
  bold-button defines draw

button new CONSTANT foo
: footext S" thin foo" ;   footext foo init
bold-button new CONSTANT bar
: bartext S" FAT BAR" ;   bartext bar init
1 bar y !

: giveitatry PAGE
  foo draw
  bar draw ;
giveitatry

\ end Bernd's application example.


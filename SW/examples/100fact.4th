\ Michel Jean. December 11, 2021.

VARIABLE resultat 10000 CELLS ALLOT 
VARIABLE grandeur_res 
VARIABLE reste
VARIABLE produit
VARIABLE x
VARIABLE n

\ Glue code for Z79Forth.
: >= < 0= ;

: multiplie ( -- ) \ renvoie la grandeur du resultat
  0 reste !
  grandeur_res @ 0 DO
    I CELLS resultat + @ x @  * reste @ + produit !
    produit @ 10 MOD resultat I CELLS + !
    produit @ 10 / reste !
  LOOP 
    
  BEGIN
    reste @ 0 >
  WHILE
    reste @ 10 MOD resultat grandeur_res @ CELLS + !
    reste @ 10 / reste !
    grandeur_res @ 1+ grandeur_res !
  REPEAT ;

: affichage ( -- )
  ." La factorielle de " n ? ." est : " CR
  grandeur_res @ 1- grandeur_res !
  BEGIN
    grandeur_res @ 0 >=
  WHILE
    grandeur_res @ cells resultat + ? 
    grandeur_res @ 1- grandeur_res !
    grandeur_res @ 30 MOD 0 = IF CR THEN \ 30 car. par ligne
  REPEAT ;

: factorielle ( -- )
  n !
  1 resultat 0 CELLS + !
  1 grandeur_res !
  2 x !
    
  BEGIN 
    x @ n @ <=
  WHILE 
    multiplie
    x @ 1+ x !
  REPEAT
  affichage ;

TICKS U. U. CR 100 factorielle CR TICKS U. U.


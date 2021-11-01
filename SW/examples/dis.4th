\ The bare minimum to be able to make sense of
\ the generated code.

9 CONSTANT tab

\ Insert a counted string into the dictionary
: " [CHAR] " WORD C@ 1+ ALLOT ;

CREATE opc-table
\ One record per opcode. Record structure is as follows:
\ 1 byte: opcode value
\ 1 byte: operand size
\ counted string: source code
$BD C,  2 C,  " jsr"
$8E C,  2 C,  " lxi"
$7E C,  2 C,  " jmp"
$39 C,  0 C,  " rts"
$25 C,  1 C,  " bcs"
$26 C,  1 C,  " bne"
$C7 C,  0 C,  " illopc"         \ Illegal opcode (debug mode)
0   C, -1 C,  " fcb"            \ End of table. Catchall option
MONITOR

: instr>opsize ( table-addr -- table-addr opsize ) DUP 1+ C@ ;
: instr>source ( table-addr -- table-addr srcaddr ) DUP 2+ ;

: opcode-lookup ( opcode -- table-addr )
  >R opc-table BEGIN
    DUP C@ 0=                   \ End of table?
    OVER C@ R@ = OR IF          \ Opcode match?
      R> DROP EXIT
    THEN
    2+ DUP C@ 1+ +              \ Point to the next record
  AGAIN ;

: hdmp ( value nbytes -- ) BASE @ >R HEX
  0 SWAP 0 <# DO # # LOOP #> TYPE  R> BASE ! ;
: .tab tab EMIT ;
: .src ( table-addr -- table-addr ) instr>source COUNT TYPE ;

: dis ( nbytes -- )
  FIND ?DUP UNLESS
    ." Undefined word" EXIT     \ Target word must exist
  THEN

  PAYLOAD SWAP DUP ( nbytes\addr\addr ) ROT + SWAP DO
    CR I 2 hdmp SPACE           \ Hex dump the address
    I C@ 1 hdmp                 \ Opcode hex dump

    I C@ opcode-lookup          \ table-addr
    instr>opsize $FF = IF       \ Data (FCB)
      .tab  .tab  .src  .tab
      [CHAR] $ EMIT
      I C@ 1 hdmp               \ Dump byte at address I
    ELSE
      instr>opsize 2 = IF       \ LDX/JSR/JMP
        I 1+ @ 2 hdmp  .tab     \ Operand hex dump
        .src  .tab
        I 1+ @ .'               \ Dump the operand through .'
        R> 2+ >R                \ Skip the two byte operand
      ELSE
        instr>opsize 1 = IF     \ Relative branch
          I 1+ C@ 1 hdmp  .tab  \ Operand hex dump
          .src  .tab
          \ Dump the byte at I+1 and add 2 to it
          ." *+" I 1+ C@ 2+ 1 hdmp
          R> 1+ >R              \ Skip the one byte operand
        ELSE                    \ No operand
          .tab  .tab  .src
        THEN
      THEN
    THEN
    DROP
  LOOP ;


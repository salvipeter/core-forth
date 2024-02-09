: .S ( -- ) DEPTH 0 ?DO DEPTH I - 1- PICK . BL EMIT LOOP CR ;

: FIND-NAME ( xt -- c-addr )
  >R 1 CELLS @ BEGIN DUP R@ > WHILE @ REPEAT R> DROP CELL+ ;

: FORGET-NAME ( "<spaces>name" -- )
  ' FIND-NAME 0 SWAP C! ;

\ Shows until the first EXIT command;
\ consequently it may fail for some words
\ (notably for those of the virtual machine).
\ Since literals may be execution tokens,
\ it displays them as such if there is an exact match.
: SEE ( "<spaces>name" -- ) ;

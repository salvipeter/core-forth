: .S ( -- )
  [CHAR] < EMIT BL EMIT
  DEPTH 0 ?DO DEPTH I - 1- PICK . BL EMIT LOOP
  [CHAR] > EMIT ;

: FIND-HEADER ( xt -- c-addr )
  >R 1 CELLS @ BEGIN DUP R@ > WHILE @ REPEAT R> DROP ;

: FORGET-NAME ( "<spaces>name" -- )
  ' FIND-HEADER CELL+ 0 SWAP C! ;

\ Shows until the first EXIT command;
\ consequently it may fail for some words
\ (notably for those of the virtual machine).
\ Since literals may be execution tokens,
\ it displays them as such if there is an exact match.
: SEE ( "<spaces>name" -- )
  BL WORD DUP FIND                              \ c-addr body flag
  ?DUP 0= IF ." word not found" 2DROP EXIT THEN \ c-addr body flag
  >R CR ." : " SWAP COUNT TYPE                  \ body ; R: flag
  BEGIN
    BL EMIT
    DUP @ [ ' EXIT ] LITERAL <> WHILE
    DUP @ -14 = IF ." <LIT>" ELSE
      DUP @ DUP FIND-HEADER 2DUP >BODY          \ body xt? head xt? xt
      = IF CELL+ DUP 1+ SWAP @ 31 AND TYPE DROP ELSE DROP . THEN
    THEN CELL+
  REPEAT ." ;" R> 1 = IF ."  IMMEDIATE" THEN CR ;

.( [2J[0;0HWelcome to Core Forth!)

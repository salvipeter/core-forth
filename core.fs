: CELL+ 1 CELLS + ;
: CHAR+ 1 + ;
: CHARS ;

: HERE  0 @ ;
: >IN   5 CELLS ;
: STATE 9 CELLS ;

: 1+  1 + ;
: 1- -1 + ;

: DUP 7 CELLS @ CELL+ @ ;
: OVER 7 CELLS @ 2 CELLS + @ ;
: DROP 7 CELLS @ 2 CELLS + 7 CELLS ! ;
: SWAP OVER OVER 7 CELLS @ 4 CELLS + ! 7 CELLS @ 2 CELLS + ! ;

: R@ 3 CELLS @ 2 CELLS + @ ;
: >R R@ SWAP 3 CELLS @ CELL+ ! 3 CELLS @ ! ;
: R> 3 CELLS @ 2 CELLS + DUP @ SWAP R@ SWAP ! 3 CELLS DUP @ 2 CELLS + SWAP ! ;

: ROT >R SWAP R> SWAP ;
: TUCK DUP ROT SWAP ;
: NIP SWAP DROP ;

: 2>R R> ROT >R SWAP >R >R ;
: 2R@ R> R> R@ SWAP DUP >R ROT >R ;
: 2R> R> R> R> SWAP ROT >R ;

: 2DUP OVER OVER ;
: 2DROP DROP DROP ;
: 2SWAP >R ROT ROT R> ROT ROT ;
: 2OVER 2SWAP 2>R 2R@ 2SWAP 2R> ;

: INVERT DUP NAND ;
: NEGATE INVERT 1+ ;
: AND NAND INVERT ;
: OR INVERT SWAP INVERT NAND ;
: XOR 2DUP OR ROT ROT NAND AND ;
: FALSE 0 ;
: TRUE -1 ;
: - NEGATE + ;
: 0> NEGATE 0< ;
: 0= DUP 0< SWAP 0> OR INVERT ;
: = - 0= ;
: <> = INVERT ;
: 0<> 0= INVERT ;

: C@ @ 255 AND ;
: C! DUP @ 255 INVERT AND ROT + SWAP ! ;

: ALLOT HERE + 0 ! ;
: , HERE ! HERE CELL+ 0 ! ;
: C, HERE C! HERE 1+ 0 ! ;
: +! SWAP OVER @ + SWAP ! ;
: 2! SWAP OVER ! CELL+ ! ;
: 2@ DUP CELL+ @ SWAP @ ;

: IMMEDIATE 1 CELLS @ CELL+ DUP @ 128 OR SWAP ! ;
: [ FALSE STATE ! ; IMMEDIATE
: ] TRUE STATE ! ;
: COMPILE, , ;
: LIT@ R> DUP CELL+ >R @ ;
: LITERAL LIT@ LIT@ , , ; IMMEDIATE

: JUMP R> DUP @ + >R ;
: ?JUMP 0= R@ @ -1 CELLS + AND R> + CELL+ >R ;
: JUMP! LIT@ DUP , LIT@ HERE , LIT@ SWAP , LIT@ - ,
        LIT@ LIT@ , , LIT@ + , LIT@ SWAP , LIT@ ! , ;
: IF LIT@ ?JUMP , HERE 0 , ; IMMEDIATE
: ELSE LIT@ JUMP , HERE 0 , SWAP [ 0 JUMP! ] ; IMMEDIATE
: THEN [ 0 JUMP! ] ; IMMEDIATE

: BEGIN HERE ; IMMEDIATE
: AGAIN LIT@ JUMP , HERE - , ; IMMEDIATE
: UNTIL LIT@ ?JUMP , HERE - , ; IMMEDIATE
: WHILE LIT@ ?JUMP , HERE 0 , SWAP ; IMMEDIATE
: REPEAT LIT@ JUMP , HERE - , [ 0 JUMP! ] ; IMMEDIATE

: DO HERE LIT@ >R , LIT@ >R , R> 0 >R >R ; IMMEDIATE
: ?DO HERE LIT@ 2DUP , LIT@ = , LIT@ ?JUMP , 4 CELLS , LIT@ 2DROP ,
      LIT@ JUMP , R> HERE 0 , LIT@ >R , LIT@ >R , >R 1 >R >R ; IMMEDIATE
: UNLOOP R> R> R> 2DROP >R ;
: LEAVE LIT@ UNLOOP , LIT@ JUMP , R> R> 1+ HERE 0 , >R >R >R ; IMMEDIATE
: LOOP R> R> BEGIN DUP 0<> WHILE 1- R> [ 8 CELLS JUMP! ] REPEAT
       DROP >R LIT@ R> , LIT@ R> , LIT@ 1+ , LIT@ 2DUP , LIT@ = ,
       LIT@ ?JUMP , HERE - , LIT@ 2DROP , ; IMMEDIATE
: I R> R> R@ SWAP >R SWAP >R ;
: J R> R> R> R> R@ SWAP >R SWAP >R SWAP >R SWAP >R ;

: CASE 0 ; IMMEDIATE
: OF LIT@ OVER , LIT@ = , LIT@ ?JUMP , HERE 0 , LIT@ DROP , ; IMMEDIATE
: ENDOF LIT@ JUMP , HERE 0 , SWAP [ 0 JUMP! ] SWAP 1+ ; IMMEDIATE
: ENDCASE LIT@ DROP , 0 ?DO [ 0 JUMP! ] LOOP ; IMMEDIATE

: < 2DUP 0< >R 0< >R - 0< R> R> IF AND ELSE OR THEN ;
: > SWAP < ;
: ABS DUP 0< IF NEGATE THEN ;
: MIN 2DUP > IF SWAP THEN DROP ;
: MAX 2DUP < IF SWAP THEN DROP ;
: S>D DUP 0< IF -1 ELSE 0 THEN ;
: DABS DUP 0< IF INVERT SWAP DUP 0= IF DROP 1+ 0 ELSE NEGATE THEN SWAP THEN ;
: SM/REM OVER >R >R DABS R@ ABS UM/MOD R> R@ XOR
         0< IF NEGATE THEN SWAP R> 0< IF NEGATE THEN SWAP ;
: FM/MOD DUP >R SM/REM OVER DUP 0<> SWAP R@ XOR 0< AND
         IF 1 - SWAP R> + SWAP ELSE R> DROP THEN ;
: /MOD >R S>D R> SM/REM ;
: / /MOD NIP ;
: MOD /MOD DROP ;

: ALIGNED DUP 1 CELLS MOD DUP IF - CELL+ ELSE DROP THEN ;
: ALIGN HERE DUP ALIGNED SWAP - ALLOT ;
: NAME>INTERPRET CELL+ DUP @ 31 AND + 1+ ALIGNED ;
: RECURSE 10 CELLS @ NAME>INTERPRET , ; IMMEDIATE

: ?DUP DUP IF DUP THEN ;
: DEPTH 8 CELLS @ 7 CELLS @ - 1 CELLS / 2 - ;
: PICK DUP 0= IF DROP DUP EXIT THEN SWAP >R 1- RECURSE R> SWAP ;
: ROLL DUP 0= IF DROP EXIT THEN SWAP >R 1- RECURSE R> SWAP ;

: FILL SWAP >R SWAP R> 0 ?DO 2DUP C! 1+ LOOP 2DROP ;
: MOVE >R 2DUP <
       IF R@ + SWAP R@ + SWAP R> 0 ?DO SWAP 1- SWAP 1- 2DUP SWAP C@ SWAP C! LOOP
       ELSE R> 0 ?DO 2DUP SWAP C@ SWAP C! SWAP 1+ SWAP 1+ LOOP
       THEN 2DROP ;
: ERASE 0 FILL ;
: UNUSED 2 CELLS @ HERE - ;

: BASE 18 CELLS ;
: DECIMAL 10 BASE ! ;
: HEX 16 BASE ! ;
DECIMAL

FALSE 11 CELLS !
: SOURCE-ID 11 CELLS @ ;
: SOURCE SOURCE-ID IF 13 CELLS @ 14 CELLS @ ELSE 4 CELLS @ 6 CELLS @ THEN ;
: REFILL SOURCE-ID IF
           FALSE
         ELSE
           4 CELLS @ 7 CELLS @ OVER - ACCEPT 6 CELLS ! 0 >IN ! TRUE
         THEN ;
: SAVE-INPUT >IN @ ;
: RESTORE-INPUT SOURCE-ID IF >IN ! FALSE ELSE DROP TRUE THEN ;
: \ SOURCE >IN ! DROP ; IMMEDIATE

\ At last I can speak!

\ WORD below uses a scratch region that starts at the end of user memory;
\ we should leave room for that, as well as for a padding region.
\ Around 200 bytes each should suffice.

: PAD [ 2 CELLS @ 200 ALIGNED - ] LITERAL ;
PAD 200 ALIGNED - 2 CELLS !           \ User memory ends where scratch begins

: BL 32 ;
: CR 10 EMIT ;
: SPACE BL EMIT ;
: SPACES 0 ?DO SPACE LOOP ;
: TYPE 0 ?DO DUP C@ EMIT 1+ LOOP DROP ;
: COUNT DUP C@ SWAP 1+ SWAP ;
: WORD 2 CELLS @ DUP 1+ ROT                    \ dest dest+1 char
       SOURCE >IN @ ?DO 2DUP I + C@ <>
                        IF LEAVE THEN 1 >IN +!
                    LOOP DROP                  \ dest dest+k char
       SOURCE >IN @ ?DO 1 >IN +! 2DUP I + C@ DUP ROT =
                        IF DROP LEAVE THEN
                        2>R SWAP R> OVER C! 1+ SWAP R>
                    LOOP 2DROP                 \ dest dest+n
       OVER - 1- OVER C! ;
: CHAR BL WORD 1+ C@ ;
: [CHAR] LIT@ LIT@ , CHAR , ; IMMEDIATE
: ( SOURCE >IN @ ?DO
      1 >IN +! DUP I + C@ [CHAR] ) = IF LEAVE THEN
    LOOP DROP ; IMMEDIATE

: COMPARE ( c-addr1 u1 c-addr2 u2 -- n )
  ROT 2DUP >R >R MIN       \ c-addr1 c-addr2 umin ; R: u1 u2
  0 ?DO 2DUP C@ SWAP C@ <> \ c-addr1 c-addr2 flag ; R: u1 u2
        IF C@ SWAP C@ > IF -1 ELSE 1 THEN UNLOOP 2R> 2DROP EXIT THEN
        1+ SWAP 1+ SWAP
    LOOP 2DROP 2R>         \ u1 u2
  2DUP < IF -1 ELSE 2DUP > IF 1 ELSE 0 THEN THEN >R 2DROP R> ;
: FIND >R 1 CELLS @ \ a-addr ; R: c-addr
       BEGIN
         DUP 0= IF R> SWAP EXIT THEN
         DUP CELL+ DUP 1+ SWAP C@ 31 AND R@ COUNT COMPARE WHILE @
       REPEAT
       R> DROP DUP NAME>INTERPRET SWAP CELL+ C@ 128 AND 0= IF -1 ELSE 1 THEN ;
: ' BL WORD FIND DROP ;
: ['] LIT@ LIT@ , ' , ; IMMEDIATE
: EXECUTE >R ;
: [COMPILE] ' , ; IMMEDIATE
: POSTPONE BL WORD FIND 1 = IF , ELSE LIT@ LIT@ , , LIT@ , , THEN ; IMMEDIATE

: PARSE ( char "ccc<char>" -- c-addr u )
  >IN @ SWAP OVER SOURCE ROT ?DO          \ >in char c-addr
    1 >IN +! 2DUP I + C@ = IF LEAVE THEN
  LOOP NIP OVER >R + >IN @ R> - 1- ;
: PARSE-NAME ( "<spaces>name<space>" -- c-addr u )
  SOURCE >IN @ ?DO DUP I + C@ BL <> IF LEAVE THEN 1 >IN +! LOOP DROP
  >IN @ 0                                         \ >in correction
  SOURCE >IN @ ?DO 1 >IN +! DUP I + C@ BL
                   = IF SWAP 1- SWAP LEAVE THEN
               LOOP                               \ >in correction c-addr
  SWAP >R OVER >R + >IN @ R> - R> + ;

: C" [CHAR] " PARSE                           \ c-addr u
     POSTPONE JUMP DUP 1+ ALIGNED DUP CELL+ , \ c-addr u offset
     >R DUP HERE C! HERE 1+ SWAP MOVE R>      \ offset
     DUP ALLOT HERE SWAP - POSTPONE LITERAL ; IMMEDIATE
: S" [CHAR] " PARSE                        \ c-addr u
     POSTPONE JUMP DUP ALIGNED DUP CELL+ , \ c-addr u offset
     >R >R HERE R@ MOVE R> R>              \ u offset
     DUP ALLOT HERE SWAP - POSTPONE LITERAL POSTPONE LITERAL ; IMMEDIATE
: ." POSTPONE S" POSTPONE TYPE ; IMMEDIATE
: .( [CHAR] ) PARSE TYPE ; IMMEDIATE

: WITHIN ( n1|u1 n2|u2 n3|u3 -- flag )
  2DUP > >R >R OVER > INVERT SWAP R> < R> IF OR ELSE AND THEN ;

: M* ( n1 n2 -- d ) DUP 0< IF NEGATE -1 ELSE 1 THEN ROT \ |n2| sign n1
     DUP 0< IF NEGATE SWAP NEGATE SWAP THEN ROT UM* ROT \ |n1|*|n2| sign
     0< IF INVERT SWAP ?DUP 0= IF 1+ 0 ELSE NEGATE THEN SWAP THEN ;
: * M* DROP ;
: */MOD >R M* R> SM/REM ;
: */ */MOD NIP ;
: U< 2DUP * 0= IF 0<> NIP ELSE 2DUP 0< SWAP 0< XOR IF DROP 0> ELSE < THEN THEN ;
: U> SWAP U< ;

: 2* 2 UM* DROP ;
: LSHIFT 0 ?DO 2* LOOP ;
1 8 CELLS 1- LSHIFT 15 CELLS !
: 2/ DUP 0 2 UM/MOD NIP SWAP 0< IF 15 CELLS @ OR THEN ; \ signed right shift
: RSHIFT 0 ?DO 0 2 UM/MOD NIP LOOP ;

\ This is very complicated, because it needs to work for both
\ signed and unsigned parameters.
\ From a discussion at forth-standard.com:
\   [..] compute x=(index-limit)+minint, and observe if the addition x+n
\        crosses the boundary between minint and maxint.
\ Note that minint is now stored in cell 15, and maxint = minint - 1.
: +LOOP R> R> BEGIN DUP 0<> WHILE 1- R> [ 29 CELLS JUMP! ] REPEAT DROP >R
        POSTPONE    R> POSTPONE   R> POSTPONE   ROT    POSTPONE     >R
        POSTPONE  2DUP POSTPONE SWAP POSTPONE     - 15 POSTPONE LITERAL
        POSTPONE CELLS POSTPONE    @ POSTPONE     +    POSTPONE    DUP
        POSTPONE   DUP POSTPONE   R@ POSTPONE     +    POSTPONE    XOR
        POSTPONE    0< POSTPONE SWAP POSTPONE    R@    POSTPONE    XOR
        POSTPONE    0> POSTPONE  AND POSTPONE  SWAP    POSTPONE     R>
        POSTPONE     + POSTPONE SWAP POSTPONE ?JUMP        HERE - ,
        POSTPONE 2DROP ; IMMEDIATE

: >NUMBER ( ud1 c-addr1 u1 -- ud2 c-addr2 u2 )
  DUP 0= IF EXIT THEN OVER C@
  DUP [CHAR] 0 [CHAR] 9 1+ WITHIN IF [CHAR] 0 - ELSE
  DUP [CHAR] A [CHAR] Z 1+ WITHIN IF [CHAR] A - 10 + ELSE
  DUP [CHAR] a [CHAR] z 1+ WITHIN IF [CHAR] a - 10 + ELSE
  DROP EXIT THEN THEN THEN
  BASE @ OVER > IF     \ ud1 c-addr1 u1 n
    SWAP >R 2>R BASE @ \ ud1-lo ud1-hi base ; R: u1 c-addr n
    SWAP OVER * ROT ROT UM* ROT +                     \ multiply ud1 with base
    R> ROT >R R@ + DUP R> < IF SWAP 1+ ELSE SWAP THEN \ and add n to the product
    R> 1+ R> 1- RECURSE
  ELSE DROP THEN ;

\ Very similar to S" but with backlash substitutions
: S\" [CHAR] " PARSE POSTPONE JUMP HERE 0 , >R OVER + >R
      BEGIN \ c-addr ; R: size-addr end-addr
        DUP C@ [CHAR] \ = IF
          1+ DUP C@ CASE
            [CHAR] a OF  7 ENDOF
            [CHAR] b OF  8 ENDOF
            [CHAR] e OF 27 ENDOF
            [CHAR] f OF 12 ENDOF
            [CHAR] l OF 10 ENDOF
            [CHAR] m OF 13 C, 10 ENDOF
            [CHAR] n OF 10 ENDOF
            [CHAR] q OF 34 ENDOF
            [CHAR] r OF 13 ENDOF
            [CHAR] t OF  9 ENDOF
            [CHAR] v OF 11 ENDOF
            [CHAR] z OF  0 ENDOF
            [CHAR] " OF 34 ENDOF
            [CHAR] x OF 1+ BASE @ HEX OVER 0 0 ROT 2 \ c-addr base 0 c-addr 2
                        >NUMBER 2DROP DROP >R BASE ! 1+ R> ENDOF
            [CHAR] \ OF 92 ENDOF
            ." invalid escape sequence" CR
          ENDCASE
        ELSE DUP C@ THEN C, 1+ DUP R@ < WHILE
      REPEAT DROP R> DROP R>                     \ size-addr
      HERE OVER - 1 CELLS - >R ALIGN HERE OVER - \ size-addr a-len ; R: len
      DUP ROT ! HERE SWAP - CELL+                \ addr ; R: len
      POSTPONE LITERAL R> POSTPONE LITERAL ; IMMEDIATE

: HOLD 16 CELLS DUP @ 1- DUP ROT ! C! ;
: HOLDS >R 16 CELLS DUP @ R@ - DUP ROT ! R> MOVE ;
: <# PAD 16 CELLS ! ;
: # BASE @ >R 0 R@ UM/MOD R> SWAP >R UM/MOD R>    \ = BASE @ UD/MOD
    ROT DUP 10 < IF [CHAR] 0 + ELSE [CHAR] A + 10 - THEN HOLD ;
: #S BEGIN # 2DUP OR 0= UNTIL ;
: #> 2DROP 16 CELLS @ PAD OVER - ;
: SIGN 0< IF [CHAR] - HOLD THEN ;
: .R >R DUP ABS 0 <# #S ROT SIGN #> \ c-addr u ; R: len
     DUP R@ < IF R> OVER - SPACES ELSE R> DROP THEN TYPE BL EMIT ;
: U.R >R 0 <# #S #> DUP R@ < IF R> OVER - SPACES ELSE R> DROP THEN
      TYPE BL EMIT ;
: . 0 .R ;
: U. 0 U.R ;

: ENVIRONMENT? 2DROP FALSE ;

\ Create a header for :NONAME as well (for RECURSE to work)
: :NONAME HERE 10 CELLS ! 0 , 0 , TRUE STATE ! FALSE ; \ FALSE on the stack
: ; POSTPONE EXIT 10 CELLS @ SWAP \ a-addr colon-sys
  IF 1 CELLS 2DUP @ SWAP ! ! ELSE NAME>INTERPRET THEN FALSE STATE ! ; IMMEDIATE
TRUE \ Leave a true here, because the VM implementation of : does not
: : HERE 10 CELLS ! 1 CELLS @ , PARSE-NAME DUP C, >R     \ c-addr ; R: u
         HERE R@ MOVE R> ALLOT ALIGN TRUE STATE ! TRUE ; \ TRUE on the stack

: CREATE : HERE 4 CELLS + POSTPONE LITERAL POSTPONE ; 0 , ;
: >BODY 4 CELLS + ;
: DOES> 1 POSTPONE LITERAL POSTPONE CELLS POSTPONE @ POSTPONE NAME>INTERPRET
        2 POSTPONE LITERAL POSTPONE CELLS POSTPONE + [ ' JUMP ] LITERAL
        POSTPONE LITERAL POSTPONE OVER POSTPONE ! POSTPONE CELL+ HERE 7 CELLS +
        POSTPONE LITERAL POSTPONE OVER POSTPONE - POSTPONE SWAP POSTPONE !
        POSTPONE EXIT ; IMMEDIATE
: VARIABLE CREATE 0 , ;
: CONSTANT CREATE , DOES> @ ;
: BUFFER: CREATE ALLOT ;
: MARKER CREATE 1 CELLS @ @ , DOES> @ 1 CELLS ! ;

: DEFER CREATE 0 , DOES> @ EXECUTE ;
: DEFER@ 3 CELLS + @ ;
: DEFER! 3 CELLS + ! ;
: IS STATE @ IF ( compilation ) ' POSTPONE LITERAL POSTPONE DEFER!
     ELSE ( interpretation ) ' DEFER! THEN ; IMMEDIATE
: ACTION-OF STATE @ IF ( compilation ) ' POSTPONE LITERAL POSTPONE DEFER@
            ELSE ( interpretation ) ' DEFER@ THEN ; IMMEDIATE

: VALUE CREATE , DOES> @ ;
: TO STATE @ IF ( compilation ) ' 3 CELLS + POSTPONE LITERAL POSTPONE !
     ELSE ( interpretation ) ' 3 CELLS + ! THEN ; IMMEDIATE

DEFER ABORT
: ABORT" POSTPONE ?DUP POSTPONE IF
         POSTPONE ." POSTPONE CR POSTPONE ABORT POSTPONE THEN ; IMMEDIATE

\ Except for the first line, this is the same as the inner loop in QUIT.
\ It would look better as its own INTERPRET function,
\ but then it would clutter up the control stack, causing errors.
: EVALUATE >IN @ 12 CELLS ! TRUE 11 CELLS ! 0 >IN ! 14 CELLS ! 13 CELLS !
           BEGIN
             SOURCE NIP >IN @ > WHILE
             BL WORD DUP C@
             0<> IF >R 0 0 R@ COUNT OVER C@ [CHAR] - = OVER 1 > AND
                    IF -1 >R 1- SWAP 1+ SWAP ELSE 1 >R THEN >NUMBER
                    0= IF 2DROP R> * R> DROP               \ it's a number!
                          STATE @ IF POSTPONE LITERAL THEN
                       ELSE DROP 2DROP R> DROP R@ FIND     \ it's a word!
                            DUP 0= IF [CHAR] ' EMIT R> COUNT TYPE
                                      [CHAR] ' EMIT ."  not found" CR ABORT
                                   ELSE R> DROP THEN
                            0< STATE @ AND IF , ELSE EXECUTE THEN
                       THEN
                 ELSE DROP THEN
           REPEAT ;

0 17 CELLS !
: QUIT 0 >R [ HERE 18 CELLS + ] LITERAL       \ address of after the next line
       4 CELLS @ -1 CELLS + DUP ROT ROT ! 3 CELLS ! \ clears the control stack
       FALSE STATE ! FALSE 11 CELLS ! REFILL DROP
       BEGIN
         BEGIN
           SOURCE NIP >IN @ > WHILE
           BL WORD DUP C@
           0<> IF >R 0 0 R@ COUNT OVER C@ [CHAR] - = OVER 1 > AND
                  IF -1 >R 1- SWAP 1+ SWAP ELSE 1 >R THEN >NUMBER
                  0= IF 2DROP R> * R> DROP               \ it's a number!
                        STATE @ IF POSTPONE LITERAL THEN
                     ELSE DROP 2DROP R> DROP R@ FIND     \ it's a word!
                          DUP 0= IF [CHAR] ' EMIT R> COUNT TYPE
                                    [CHAR] ' EMIT ."  not found" CR ABORT
                                 ELSE R> DROP THEN
                          0< STATE @ AND IF , ELSE EXECUTE THEN
                     THEN
               ELSE DROP THEN
         REPEAT
         11 CELLS @ IF
           FALSE 11 CELLS ! 12 CELLS @ >IN !
         ELSE
           17 CELLS @ CASE
             1 OF ."  ok" CR ENDOF
             2 OF [CHAR] < EMIT BL EMIT
                  DEPTH 0 ?DO DEPTH I - 1- PICK . BL EMIT LOOP
                  [CHAR] > EMIT CR ENDOF
           ENDCASE
           REFILL DROP
         THEN
       AGAIN ;

:NONAME 8 CELLS @ 7 CELLS ! QUIT ; IS ABORT

QUIT \ Start the interpreter

\ Finally, an overview of the system variables defined here:
\ |------|-------------------------------------------------|
\ | Cell | Purpose                                         |
\ |------|-------------------------------------------------|
\ |  11  | the value returned by SOURCE-ID (FALSE / TRUE)  |
\ |  12  | saved >IN                                       |
\ |  13  | address of string to evaluate                   |
\ |  14  | length of string to evaluate                    |
\ |  15  | highest bit in a cell = 2^(n-1)                 |
\ |  16  | start address of number image                   |
\ |  17  | verbosity (0: quiet, 1: ok, 2: stack)           |
\ |  18  | radix base                                      |
\ |  19  | (not used)                                      |
\ |------|-------------------------------------------------|

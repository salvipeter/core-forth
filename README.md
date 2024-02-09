# Core Forth

This is a minimal ANSI Forth implementation. It implements the *core* and
*core extension* wordsets in the [Forth 2012 Standard](https://forth-standard.org/),
or at least its current draft.

It is minimal in two senses:

 - Other then the above two wordsets, there are only a handful of extra words (see below)
 
 - Aside from a few very basic words, everything is written in Forth itself

Characters are assumed to be 1 byte, but cell size is easily adjustable (default is 32 bits).
The system is assumed to be little-endian, and endline character is 10 (line feed).

## Credits

Based on [milliForth](https://github.com/fuzzballcat/milliForth),
which is in turn based on [sectorforth](https://github.com/cesarblum/sectorforth),
which was inspired by a [Usenet post](https://groups.google.com/g/comp.lang.forth/c/NS2icrCj1jQ)
by Bernd Paysan.

Some words are defined based on reference implementations at the
[Forth 2012 Standard website](https://forth-standard.org/).

## Extra words

The following words are not part of the *core* and *core extension* datasets:

- `COMPARE ( c-addr1 u1 c-addr2 u2 -- n )` compares two strings (part of the *string* wordset)
- `JUMP ( -- )` jumps to the relative address in the next cell
- `?JUMP ( flag -- )` jumps to the relative address in the next cell when `flag` is `0`;
  otherwise jumps over the next cell
- `JUMP! ( offset -- )` compiles the computation and storing of the current relative address
  plus the given offset (storage address is given on the data stack in runtime)
- `LIT@ ( -- x )` loads the next cell onto the data stack and jumps over it
- `NAND ( x1 x2 -- x3 )` is the bitwise NAND operation
<TODO>

An additional file (`utils.fs`) contains some standard utilities from the *tools* wordset:
- `.S ( -- )`
- `SEE ( "<spaces>name" -- )`
<TODO>

## About the virtual machine

The machine memory has the following structure:

```
<- low addresses . . . . . . high addresses ->
VVVVVMMMMMMMMMMMMMMMMMMMMMMMMMMMMRRRRRTTDDDDDD
     M->                           <-R     <-D

V: system variables
M: main memory (where the user dictionary lives)
R: return stack (grows downward)
T: text input buffer (stores the current line)
D: data stack (grows downward)
```

The first part of the memory contains the following system variables:

| Cell  | Name    | Meaning                                                  |
|-------|---------|----------------------------------------------------------|
| 0     | HERE    | start of unreserved data space                           |
| 1     | DICT    | address of top of the dictionary                         |
| 2     | MEMEND  | end of user memory                                       |
| 3     | RSP     | return stack pointer                                     |
| 4     | TIB     | text input buffer address (= bottom of the return stack) |
| 5     | >IN     | input buffer offset                                      |
| 6     | TIBSIZE | text input buffer size                                   |
| 7     | DSP     | data stack pointer                                       |
| 8     | MEMSIZE | memory size (= bottom of the data stack)                 |
| 9     | STATE   | TRUE during compilation                                  |
| 10    | LATEST  | address of the latest (started) definition               |
| 11-19 | -       | (reserved for variables in `core.fs`)                    |

The virtual machine also knows some basic operations:

- `@ ( a-addr -- x )`
- `! ( x a-addr -- )`
- `0< ( n -- flag )`
- `+ ( n1|u1 n2|u2 -- n3|u3 )`
- `* ( n1|u1 n2|u2 -- n3|u3 )`
- `/ ( n1|u1 n2|u2 -- n3|u3 )`
- `NAND ( x1 x2 -- x3 )`
- `KEY ( -- char )`
- `EMIT ( x -- )`
- `EXIT ( -- ) ( R: nest-sys -- )`

It also has a full interpreter that can parse decimal numbers, and define new words with `: ... ;`.
(This is for bootstrapping; these functionalities are rewritten later in `core.fs`.)

Note that while addition, multiplication and division could be written in Forth, 
as well, they are incorporated in the interpreter for efficiency reasons.
A naïve implementation is included in comments.

A dictionary entry has the following form:

| 1 cell  | Link to previous entry (or 0 at the first entry)                  |
| 1 bit   | 1 if immediate                                                    |
| 2 bits  | (reserved)                                                        |
| 5 bits  | name length (max. 31)                                             |
| N bytes | name                                                              |
| K bytes | aligning (K is minimal s.t. 1+N+K is a multiple of the cell size) |
| M cells | body (addresses of other bodies)                                  |

The body of system words (i.e., one of `@ ! 0< + * / NAND EXIT KEY EMIT : ;`)
is a single negative number (-1 for `@`, -2 for `!` etc.).

## Testing

The test framework and the test for the *core* wordset were developed by John Hayes,
and those for the *core extension* wordset by Gerry Jackson.

<TODO>

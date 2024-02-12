# Core Forth

This is a minimal ANSI Forth implementation. It implements the *core* and
*core extension* wordsets in the [Forth 2012 Standard](https://forth-standard.org/),
or at least its current draft.

It is minimal in two senses:

 - Other then the above two wordsets, there are only a handful of extra words (see below)
 
 - Aside from a few very basic words, everything is written in Forth itself

## Rationale

There is (almost) none. I wanted to try to implement a fairly complete Forth system by myself,
not a toy Forth like (the otherwise wonderful)
[jonesforth](https://github.com/nornagon/jonesforth/),
or the Forths built in books like [Let Over Lambda](https://letoverlambda.com/)
(one of my favorite programming texts).

I was also intrigued by the minimal implementations given in the credits below,
and wanted to know if these ideas hold even in a full system.
While I had to make some concessions (like adding the words `UM*` and `UM/MOD`
as basic words), I am quite content with the results.

That said, this is a doubly inefficient implementation:

1. It is interpreted (not compiled)

1. Even its interpreter is interpreted (not native)

The latter is because I wanted to show that even the interpreter can
be written in Forth itself; it also helps keeping the virtual machine
to a minimum (its interpreter only understands decimal numbers, and
does not handle string sources given by `EVALUATE`).

As for the former, it shouldn't be too hard to change it to a
compiler: a new basic word like `MEM ( addr -- real-addr )` would be
needed to map relative addresses to real addresses, and some `,`s
would need to be changed to `COMPILE,`s, which, in turn, should be
defined appropriately. An even more efficient implementation would
use the hardware stack, which could be done by changing a few words.

## Credits

Based on [milliForth](https://github.com/fuzzballcat/milliForth),
which is in turn based on [sectorforth](https://github.com/cesarblum/sectorforth),
which was inspired by a [Usenet post](https://groups.google.com/g/comp.lang.forth/c/NS2icrCj1jQ)
by Bernd Paysan.

Some words are defined based on reference implementations at the
[Forth 2012 Standard website](https://forth-standard.org/).

A few words are modeled after their implementation in
[Gforth](https://www.gnu.org/software/gforth/).

## Extra words

The following words are not part of the *core* and *core extension* datasets:

- `COMPARE ( c-addr1 u1 c-addr2 u2 -- n )` compares two strings (part of the *string* wordset)
- `DABS ( d -- ud )` computes the absolute value of a double cell (part of the *double* wordset)
- `JUMP ( -- )` jumps to the relative address in the next cell
- `?JUMP ( flag -- )` jumps to the relative address in the next cell when `flag` is `0`;
  otherwise jumps over the next cell
- `JUMP! ( offset -- )` compiles the computation and storing of the current relative address
  plus the given offset (storage address is given on the data stack in runtime)
- `LIT@ ( -- x )` loads the next cell onto the data stack and jumps over it
- `NAME>INTERPRET ( nt -- xt | 0 )` returns the interpretation semantics of the given name tag
   (part of the *tools extension* wordset); this implementation never returns `0`
- `NAND ( x1 x2 -- x3 )` is the bitwise NAND operation
- `READ-LINE ( -- )` reads a line from the standard input to the text input buffer

An additional file (`utils.fs`) contains some standard utilities from the *tools* wordset:
- `.S ( -- )`
- `SEE ( "<spaces>name" -- )` [very rudimentary]
It also provides a utility for selectively forgetting words by setting their name length to 0:
- `FORGET-NAME ( "<spaces>name" -- )`
- `VERBOSITY! ( 0 | 1 | 2 -- )` sets the verbosity level of the interpreter

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

- `! ( x a-addr -- )`
- `+ ( n1|u1 n2|u2 -- n3|u3 )`
- `0< ( n -- flag )`
- `@ ( a-addr -- x )`
- `CELLS ( n1 -- n2 )`
- `EMIT ( x -- )`
- `EXIT ( -- ) ( R: nest-sys -- )`
- `KEY ( -- char )` [currently not implemented in the VM]
- `NAND ( x1 x2 -- x3 )`
- `READ-LINE ( -- )`
- `UM* ( u1 u2 -- ud )`
- `UM/MOD ( ud u1 -- u2 u3 )`

It also has an interpreter that can parse decimal numbers, and define new words with `: ... ;`.
This is only for bootstrapping; the interpreter and `:`, `;` are rewritten later in `core.fs`.
Note that the VM implementation of `:` does not leave anything on the stack.

A dictionary entry has the following form:

| 1 cell  | Link to previous entry (or 0 at the first entry)                  |
| 1 bit   | 1 if immediate                                                    |
| 2 bits  | (reserved)                                                        |
| 5 bits  | name length (max. 31)                                             |
| N bytes | name                                                              |
| K bytes | aligning (K is minimal s.t. 1+N+K is a multiple of the cell size) |
| M cells | body (addresses of other bodies)                                  |

## VM implementation notes

Characters are assumed to be 1 byte, but cell size is easily adjustable (default is 32 bits).
Needed changes: redefine `cell`, `ucell`, `udcell` and `CELL_SIZE` in `vm.c`, 
and check the mixed-precision functions `fn_mult` and `fn_divmod`.

The system is assumed to be little-endian, and endline character is 10 (line feed).

The body of system words (i.e., one of
`! + 0< : ; @ CELLS EMIT EXIT KEY NAND READ-LINE UM* UM/MOD`)
is a single negative number (-1 for `!`, -2 for `+` etc. to -14 for `UM/MOD`).
Literals are loaded by a special code (-15), but this is reimplemented later in `LIT@`.

Instead of using a full-fledged readline library, input-ending newlines are avoided
using VT100 control sequences (so it looks better on a terminal that understands that).

## Testing

The test framework and the test for the *core* wordset were developed by John Hayes,
and those for the *core extension* wordset by Gerry Jackson.

<TODO>

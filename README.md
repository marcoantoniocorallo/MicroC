# MicroC

*MicroC* is a statically typed subset of the language *C*, developed as a serie of assignments for the [*Languages, Compilers and Interpreters*](https://github.com/lillo/compiler-course-unipi) course @ [*UniPi*](https://di.unipi.it/). 

The main simplification of this language with regards to C are:

- Primitive data types: it supports only integers `int`, characters `char` and booleans `bool`;

- Data structures: it supports only one-dimensional *arrays* as data structures and *pointers* as compound data types;

- Heap: there is no support for dynamic allocation of memory;

- Functions: functions can only return `void`, `int`, `bool` or `char` values;

- There is no pointer arithmetic, there is no pointer to functions and pointers and arrays are not interchangeable;

- There is no support for separate compilation: all the code must stay in a unique compilation unit; 

Despite these simplifications, MicroC is still a *Turing-complete* language, so it is expressive enough to implement complex systems. 

It provides two library functions to interact with the user:

- `void print(int n) // prints n to the stdout`

- `int getint() // get an integer from the stdin`

It also provides all types of cycles: `for`, `do-while` and, naturally, `while`. 

There is support for the initialization of variables during the declaration and for multiple declarations.
This implementation of MicroC also provides the operators for pre/post-increment `++`, pre/post-decrement `--` and for the abbreviate form of assignments `+=`, `-=`, `*=`, `/=`, `%=`.
Furthermore, MicroC provides a **strong**(er than C) *type system* and a static analysis step that recognizes and eliminates *dead code*.

You can find a detailed description of the project and of the main design choices adopted in the official [report](https://github.com/marcoantoniocorallo/MicroC/blob/main/MicroC.pdf).

---

#### Assignments Instructions

The structure of the project (i.e. the directory structure, the development environment, the starting (ambiguous) EBNF grammar, ... ) was specified from [Prof. Galletta](https://github.com/lillo) and the whole definition is available [here](https://github.com/lillo/compiler-course-unipi/tree/main/microc).

The assignments required to:

- develop the *lexer* using *Ocamllex*;

- generate the *parser* using *Menhir*;

- implement the concept of the *symbol table*;

- define and develop the *semantic analysis* for the *type checking* and the *scoping rules*;

- develop a module for the code generation for generating *LLVM* code;

- implement at least three optional points chosen from a list.
  This implementation of MicroC provides:
  
  - the operators for *pre/post-increment* `++`, *pre/post-decrement* `--` and for the *abbreviate form* of assignments `+=`, `-=`, `*=`, `/=`, `%=`;
  
  - `do-while` loop;
  
  - variable declaration with initialization;
  
  - multiple declarations;
  
  - dead-code elimination;

---

#### Requirements

To build the project your system must have:

* OCaml >= 4.12.0
* Menhir >= 20210419
* ppx_deriving >= 5.2 
* llvm >= 12.0.0

You can install the required dependencies via `opam`

```sh
$ opam install menhir ppx_deriving llvm
```

#### Usage

To build MicroC just move in the directory and run `make`. 

Once the compiler is built, you can compile your own program using the script *microcc.sh*: `./microcc.sh <source_file>`.

---

#### To-Do

- arrays as pointers; 
- multi-dimensional arrays as in C;
- seperate compilation;
- floating point arithmetic and strings as in C, i.e. null-terminated arrays of characters;
- structs, `sizeof`, bitwise and comma operators;

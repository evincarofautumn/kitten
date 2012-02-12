# Overview

**Kitten** is a minimalistic, dynamically typed, concatenative programming
language intended primarily for Web development. (This is an in-progress
implementation of that language.) Kitten is inspired by the Cat programming
language. The Kitten compiler (`kitten`) compiles Kitten programs into C, which
can be compiled and linked against the Kitten runtime library (`libkitten`) to
produce a standalone executable. Thus a Kitten program can be built anywhere you
can find a C compiler, making it ideal for shared hosts, where installing
development tools may not be possible. Kitten will eventually come with a
standard prelude of definitions to make your development life easier. To build
the compiler, you need GHC and Parsec; the runtime, GCC. Just download the
sources and run `make` with your fingers crossed. The sources also include a
shell script named `kittenc` that you can use to compile a Kitten program
`meow.kitten` into an executable named `meow`.

Kitten has three built-in types:

  * **Integer**: a signed 64-bit integral type.

  * **Float**: a double-precision floating-point number.

  * **Quotation**: a vector of boxed values, used to represent UTF-32 strings,
    heterogeneous arrays and structures, and anonymous functions.

The Kitten language itself is very simple:

    <program>    ::= <term>*
    <term>       ::= <integer> | <float> | <quotation> | <word> | <string>
                   | <definition> | <import>
    <integer>    ::= [0-9]+
    <float>      ::= [0-9]+ '.' [0-9]+
    <quotation>  ::= '[' <term>* ']'
    <word>       ::= [A-Za-z_][0-9A-Za-z_]*
    <string>     ::= '"' [^"]* '"'
    <definition> ::= "define" <word> <quotation>
    <import>     ::= "import" <string>

Comments are given in parentheses (`()`), and can be nested.

All terms essentially denote functions, and juxtaposition of terms denotes
function composition. Thus all expressions are written in terms of data flow
only, i.e., completely point-free.

There are only two special forms in the language: `define`, which introduces a
new word definition, and `import`, which imports a Kitten file for compilation
into the final executable.

Hello world:

    "Hello world!\n" write

Hello user:

    (It is customary to give a comment alongside definitions briefly describing
    the expected inputs and outputs, of the form “inputs -- outputs”.)

    define join ([a] [b] -- [a b])
      [compose]

    define greet (name --)
      ["Hello, " swap "!\n" join join]

    define prompt (prompt -- input)
      [write read_line]

    "What is your name? " prompt greet

# Definitions

The following definitions are provided by the Kitten runtime library.

## Core Definitions

  * `[A] apply`
    Evaluates the quotation at the top of the stack.

  * `[A] [B] compose`
    Returns the composition of the two quotations at the top of the stack.

  * `A dup`
    Duplicates the value at the top of the stack.

  * `A [B] [C] if`
    Applies `[B]` if `A` is true (nonzero); otherwise, applies `[C]`.

  * `A pop`
    Discards the top value on the stack.

  * `A quote`
    Returns `[A]`.

  * `A B swap`
    Swaps the two topmost values on the stack.

## Arithmetic and Conditional

  * `A B add`
    Returns the sum of `A` and `B`.

  * `A B div`
    Returns the quotient of `A` and `B`.

  * `A B eq`
    Returns whether `A` and `B` are equal.

  * `A B ge`
    Returns whether `A` is greater than or equal to `B`.

  * `A B gt`
    Returns whether `A` is greater than `B`.

  * `A isf`
    Returns whether `A` is a float.

  * `A isi`
    Returns whether `A` is an integer.

  * `A isq`
    Returns whether `A` is a quotation.

  * `A B le`
    Returns whether `A` is less than or equal to `B`.

  * `A B lt`
    Returns whether `A` is less than `B`.

  * `A B mod`
    Returns the modulus of `A` and `B`.

  * `A B mul`
    Returns the product of `A` and `B`.

  * `A B ne`
    Returns whether `A` is not equal to `B`.

  * `A B sub`
    Returns the difference of `A` and `B`.

## I/O Definitions

  * `A putc`
    Writes `A` to standard output as a character.

  * `A write`
    Writes `A` to standard output as a formatted value.

# Standard Library

The following definitions are provided by the Kitten standard library:

  * `read_line`
    Reads a line of text into a quotation.

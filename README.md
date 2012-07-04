# Overview

**Kitten** is a minimalistic, dynamically typed, concatenative programming
language. This is an in-progress implementation of that language.

The Kitten compiler (`kitten`) compiles Kitten programs into C, which can be
compiled and linked against the Kitten runtime library (`libkitten`) to produce
a standalone executable. Thus a Kitten program can be built anywhere you can
find a C compiler, making it ideal for shared hosts such as Bluehost, where
installing development tools may be difficult or impossible.

# Building

To build the compiler, you need **GHC** with the **Parsec** and **cpphs**
libraries installed (available via cabal). For the runtime, use **GCC**
or another suitable **C compiler**. Just download the sources and run `make`
with your fingers crossed. To build the compiler or runtime library
individually, run `make compiler` or `make library`, respectively. For more
information, try `make help`.

Building a Kitten program is a two-step process. First, run through `kitten`:

    $ cat > meow.ktn
    "Meow\n" write

    $ ./kitten meow.ktn > meow.c

Next, run through a C compiler, linking against `libkitten` and `libm`.

    $ gcc meow.c -o meow -lm -L. -lkitten
    $ ./meow
    Meow

The sources include a shell script named `kittenc` that does this automatically:

    $ ./kittenc meow.ktn
    $ ./meow
    Meow

Kitten sources are stripped of any extension to produce the name of the final
executable. In general, `.ktn` is preferred.

# The Language

Kitten source files are expected to be in UTF-8; the I/O functions currently
operate only in UTF-8 as well. The language has three built-in data types:

  * **Integer**: a signed 64-bit integral type. Integer literals consist of one
    or more decimal digits. For the time being, characters in text quotations
    are represented as UTF-32 code points using this type.

  * **Float**: a double-precision (64-bit) floating-point number. Floating-point
    literals consist of a decimal point preceded and followed by one or more
    decimal digits.

  * **Quotation**: a vector of boxed values, used to represent text,
    heterogeneous arrays and structures, and anonymous functions. There are two
    kinds of quotation literal: general quotations, given in square brackets
    (`[]`), and text quotations, given in double quotes (`""`).

Text quotations support the following escape sequences:

  * `\a` Alarm (BEL)

  * `\f` Formfeed (FF)

  * `\n` Linefeed/newline (LF)

  * `\r` Carriage return (CR)

  * `\t` Horizontal tab (TAB/HT)

  * `\v` Vertical tab (VT)

  * `\\` Backslash

  * `\"` Double quote

The Kitten grammar is very simple:

<pre>
    <i>program</i>    = <i>term</i>*

    <i>term</i>       = <i>integer</i>
               | <i>float</i>
               | <i>quotation</i>
               | <i>word</i>
               | <i>text</i>
               | <i>definition</i>

    <i>integer</i>    = <i>digit+</i>
    <i>float</i>      = <i>digit+</i> . <i>digit+</i>
    <i>quotation</i>  = [ <i>term*</i> ]
    <i>word</i>       = <i>symbol</i> (<i>digit</i> | <i>symbol</i>)*
    <i>text</i>       = "..."
    <i>definition</i> = define <i>word</i> <i>quotation</i>

    <i>symbol</i>     = {A-Za-z!#$%&amp;'*+-./:;&lt;=&gt;?@\\^_|~}
    <i>digit</i>      = {0-9}

    {...} = one of
    +     = one or more
    *     = zero or more
    |     = either
</pre>

Comments are given in parentheses (`()`), and can be nested:

    (This is a comment (containing (nested) parentheses).)

All terms essentially denote functions, and juxtaposition of terms denotes
function composition. Thus all expressions are written in terms of data flow
only, i.e., completely point-free. Values are immutable and boxed, so copies are
cheap—they involve only copying a pointer and incrementing a reference count.

There are only two special forms in the language: `define`, which introduces a
new word definition, and `import`, which imports a Kitten file for compilation
into the final executable.

Hello world:

    "Hello world!\n" write

Hello user:

    (It is customary to give a comment alongside definitions briefly describing
    the expected inputs and outputs, of the form “inputs -- outputs”.)

    define join ([a] [b] -- [a b])
    [ compose ]

    define greet (name --)
    [ "Hello, " swap "!\n" join join write ]

    define prompt (prompt -- input)
    [ write read_line ]

    "What is your name? " prompt greet

# Definitions

The following definitions are provided by the Kitten runtime library.

## Core Definitions

  * `apply ([A] -- A)`
    Evaluates the quotation at the top of the stack.

  * `compose ([A] [B] -- [A B])`
    Returns the composition of the two quotations at the top of the stack.

  * `dup (A -- A A)`
    Duplicates the value at the top of the stack.

  * `if (A [B] [C] -- X)`
    Applies `[B]` if `A` is true (nonzero); otherwise, applies `[C]`.

  * `pop (A -- )`
    Discards the top value on the stack.

  * `quote (A -- [A])`
    Returns `[A]`.

  * `swap (A B -- B A)`
    Swaps the two topmost values on the stack.

  * `length (quotation -- length)`
    Returns the length of the quotation atop the stack.

## Arithmetic

Integers are promoted to floats when used in arithmetic operations with floats,
which may result in a loss of precision.

  * `add (augend addend -- sum)`
    Returns the sum of two numbers.

  * `div (divisor dividend -- quotient)`
    Returns the quotient of two numbers.

  * `mod (divisor dividend -- remainder)`
    Returns the modulus (remainder of integer division) of two numbers.

  * `mul (multiplier multiplicand -- product)`
    Returns the product of two numbers.

  * `sub (minuend subtrahend -- difference)`
    Returns the difference of two numbers.

## Conditional

Value comparisons, like arithmetic operations, use type promotion. Since
integers are promoted to floats when compared with floats, they may lose
precision. Quotations use lexicographic (dictionary) ordering.

  * `eq (A B -- Boolean)`
    Returns whether two values are equal.

  * `ge (A B -- Boolean)`
    Returns whether `A` is greater than or equal to `B`.

  * `gt (A B -- Boolean)`
    Returns whether `A` is greater than `B`.

  * `le (A B -- Boolean)`
    Returns whether `A` is less than or equal to `B`.

  * `lt (A B -- Boolean)`
    Returns whether `A` is less than `B`.

  * `ne (A B -- Boolean)`
    Returns whether `A` is not equal to `B`.

## I/O Definitions

  * `A putc`
    Writes `A` to standard output as a character.

  * `A write`
    Writes `A` to standard output as a formatted value.

## Type Checking

  * `isf (A -- Boolean)`
    Returns whether `A` is a float.

  * `isi (A -- Boolean)`
    Returns whether `A` is an integer.

  * `isq (A -- Boolean)`
    Returns whether `A` is a quotation.

# Standard Library

The following definitions are provided by the Kitten standard library:

  * `read_line`
    Reads a line of text from standard input into a quotation.

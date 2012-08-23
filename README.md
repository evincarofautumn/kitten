# Overview

**Kitten** is a high-performance statically typed concatenative programming
language. This is an in-progress implementation of that language.

The Kitten compiler (`kitten`) compiles Kitten programs into C, which can be
compiled and linked against the Kitten runtime library (`libkitten`) to produce
a standalone executable. Thus a Kitten program can be built anywhere you can
find a C compiler, making it ideal for shared hosts such as Bluehost, where
installing development tools may be difficult or impossible.

# Building

To build the compiler, you need **GHC** and **Cabal**. For the runtime, use
**GCC** or another suitable **C compiler**. Just download the sources and run
`make` with your fingers crossed. To build the compiler or runtime library
individually, run `make compiler` or `make library`, respectively. For more
information, try `make help`. Please report errors and weirdnesses to [the issue
tracker](https://github.com/evincarofautumn/kitten/issues).

Building a Kitten program is a two-step process. First, run through `kitten`:

    $ cat > meow.ktn
    "Meow\n" write

    $ ./kitten meow.ktn > meow.c

Next, run through a C compiler, linking against `libkitten` and `libm`.

    $ gcc meow.c -o meow -lm -Lbuild -lkitten
    $ ./meow
    Meow

The sources include a shell script named `kittenc` that does this automatically:

    $ ./kittenc meow.ktn
    $ ./meow
    Meow

Kitten sources are stripped of any extension to produce the name of the final
executable—`.ktn` is the preferred extension for Kitten source files.

For a detailed description of the language, check out `Specification.md`. The
`tests/` directory contains Kitten code that is up to date with the compiler,
but the implementation does not yet reflect everything in the specification.

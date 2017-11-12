# The Kitten Programming Language

**Kitten** is a statically typed, [stack-based functional programming language][concatenative] designed for simplicity, speed, and safety. This is an in-progress implementation of that language, including:

 * An interactive console for testing code.

 * An interpreter.

 * A native-code compiler producing static executables. (Incomplete.)

## Resources

 * Visit the [official site][site].

 * Skim a quick [intro][intro].

 * Browse some [examples][examples].

## Building [![Build Status](https://travis-ci.org/evincarofautumn/kitten.svg?branch=master)](https://travis-ci.org/evincarofautumn/kitten)

To build the latest compiler, you need [Stack]:

```
git clone https://github.com/evincarofautumn/kitten.git
cd kitten
stack setup  # only necessary on first build
stack build

stack exec kitten
stack exec kitten -- <flags>
# OR
stack install
~/.local/bin/kitten
~/.local/bin/kitten <flags>
```

## Miscellany

Kitten is distributed under the terms of the [MIT license][license]. Contributors should agree to abide by the [code of conduct].

[concatenative]: http://concatenative.org/
[examples]: https://github.com/evincarofautumn/kitten/tree/master/examples
[intro]: http://kittenlang.org/intro/
[site]: http://kittenlang.org/
[Stack]: https://docs.haskellstack.org/en/stable/README/
[license]: https://github.com/evincarofautumn/kitten/blob/master/LICENSE.md
[code of conduct]: https://github.com/evincarofautumn/kitten/blob/master/CODE_OF_CONDUCT.md

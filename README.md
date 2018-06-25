# The Kitten Programming Language

[![Gitter chat](https://badges.gitter.im/gitterHQ/gitter.png)][gitter] [![Build Status](https://travis-ci.org/evincarofautumn/kitten.svg?branch=master)](https://travis-ci.org/evincarofautumn/kitten)

**Kitten** is a statically typed, [stack-based functional programming language][concatenative] designed for simplicity, speed, and safety. This is an in-progress implementation of that language, including:

 * An interactive console for testing code

 * An interpreter

 * A native-code compiler producing static executables (incomplete)

## Contributing

I need help to make Kitten a reality! If you’re interested in helping in any way, you’re more than welcome, even if you’re not experienced with Haskell or compiler development. You can look at the project for the [initial release] to see what I’m working on, and check out the [contribution guidelines][contributing] for suggestions on how you can help.

## Resources

 * Browse some [examples][examples] to get a feel for the language

 * Join the [chat room][gitter] to ask questions

 * Skim a quick [intro][intro] on the [official site][site]

 * Read the most recent updates to the [ebook][ebook]

 * Read my article introducing some interesting things about concatenation programming, [Why Concatenative Programming Matters][wcpm]

 * Watch my lecture describing the theory, history, and implementation techniques of the paradigm, [Concatenative Programming: From Ivory to Metal][cpim]

## Building

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
[wcpm]: http://evincarofautumn.blogspot.com/2012/02/why-concatenative-programming-matters.html
[cpim]: https://www.youtube.com/watch?v=_IgqJr8jG8M
[ebook]: https://www.gitbook.com/book/evincarofautumn/programming-with-kitten
[initial release]: https://github.com/evincarofautumn/kitten/projects/1
[contributing]: https://github.com/evincarofautumn/kitten/blob/master/CONTRIBUTING.md
[gitter]: https://gitter.im/kittenlang/Lobby

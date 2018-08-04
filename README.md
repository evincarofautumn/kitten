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

 * Read my article introducing some interesting things about concatenative programming, [Why Concatenative Programming Matters][wcpm]

 * Watch my lecture describing the theory, history, and implementation techniques of the paradigm, [Concatenative Programming: From Ivory to Metal][cpim]

## Building and Installing

If you’re building the compiler just to try it out or work on it, you can follow the preferred build method of using [Stack]:

```
git clone https://github.com/evincarofautumn/kitten.git
cd kitten
stack setup  # only necessary on first build
stack build

stack exec kitten
stack exec kitten -- <flags>
```

However, if you want to *install* Kitten in a standard location outside the build directory, due to a deficiency in Stack’s support for Cabal’s `data-files` feature, it is **not** recommended to use `stack install` to install a copy of the executable, because this will not install the *common vocabulary* `common.ktn` containing Kitten’s standard library.

There are two workarounds. One is to forgo Stack, and build and install Kitten using Cabal directly:

```
cabal sandbox init
cabal install --only-dependencies
cabal install --prefix="$HOME/.local"
```

This will correctly install the common vocab so that Kitten can find it. The preferred install location for Kitten is `~/.local` on Unix-like systems (so the executable resides at `~/.local/bin/kitten`) or `%APPDATA%\local` on Windows (resp. `%APPDATA%\local\bin\kitten.exe`).

The other option is to manually copy `common.ktn` to the install directory:

```
stack install
cp common.ktn ~/.local/bin/
```

It’s also recommended to add the install directory (`~/.local/bin` or `%APPDATA\local\bin`) to your `PATH` so that you can invoke `kitten` directly without a path prefix.

These are the only files installed by Kitten, so to uninstall it, you only need to delete the compiler and common vocab from the install directory.

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
[ebook]: https://evincarofautumn.gitbooks.io/programming-with-kitten/
[initial release]: https://github.com/evincarofautumn/kitten/projects/1
[contributing]: https://github.com/evincarofautumn/kitten/blob/master/CONTRIBUTING.md
[gitter]: https://gitter.im/kittenlang/Lobby

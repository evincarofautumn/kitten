# The Kitten Programming Language

**Kitten** is a statically typed, [stack-based functional programming language][concatenative] designed for simplicity, speed, and safety. This is an in-progress implementation of that language, including:

 * An interactive console for testing code.

 * An interpreter.

 * A compiler targeting C99.

# Resources

**Note: these are currently out of date with the new compiler.**

 * Visit the [official site][site].

 * Browse some [examples][examples].

 * Skim a quick [intro][intro].

 * Read a (work in progress) [tutorial][tutorial].

# Building [![Build Status](https://travis-ci.org/evincarofautumn/kitten.png?branch=master)](https://travis-ci.org/evincarofautumn/kitten)

To build the latest compiler, you need [Stack]:

```
git clone https://github.com/evincarofautumn/kitten.git
cd kitten
stack setup
stack build

stack exec kitten
# OR
stack install
~/.local/bin/kitten
```

---

To build the old compiler, which will be removed soon, you need GHC ≥7.10:

```
git clone https://github.com/evincarofautumn/kitten.git
cd kitten
make
```

You can run the integration tests concurrently with `make -jn`, where `n` is the number of concurrent jobs. If you are working on the compiler, you can use `make dev` to avoid checking the library dependencies on every rebuild.

[concatenative]: http://concatenative.org/
[examples]: https://github.com/evincarofautumn/kitten/tree/master/examples
[intro]: http://kittenlang.org/intro/
[site]: http://kittenlang.org/
[tutorial]: http://kittenlang.org/tutorial/
[Stack]: https://docs.haskellstack.org/en/stable/README/

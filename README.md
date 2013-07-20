# The Kitten Programming Language

**Kitten** is a statically typed, stack-based functional programming language with concatenative semantics. This is an in-progress implementation of that language.

 * Visit the [official site][site].

 * Browse some [examples][examples].

 * Read a (work in progress) [tutorial][tutorial].

The original implementation was compiled to C. In the interest of faster iteration on language features, the current implementation is interpreted.

# Building

To build the compiler, you need GHC ≥7.4. Simply run:

```
git clone git://github.com/evincarofautumn/kitten.git
cd kitten
make all
```

[examples]: https://github.com/evincarofautumn/kitten/tree/master/examples
[site]: http://kittenlang.org/
[tutorial]: http://kittenlang.org/tutorial/

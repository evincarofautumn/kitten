# The Kitten Programming Language

**Kitten** is a minimalistic, statically typed, functional programming language
with concatenative semantics. This is an in-progress implementation of that
language.

 * Visit the [official site][1].

 * Read a [short intro][2].

 * Subscribe to the [mailing list][3].

The original implementation was compiled to C. In the interest of faster
iteration on language features, the current implementation is interpreted.

# Building

To build the compiler, you need GHC ≥7.4. Simply run:

```
git clone git://github.com/evincarofautumn/kitten.git
cd kitten
make all
```

[1]: http://kittenlang.org/
[2]: http://kittenlang.org/intro/
[3]: http://kittenlang.org/kitteneers/

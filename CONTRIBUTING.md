# Contributing

Thanks for your interest in contributing to Kitten! I hope this guide will give you an idea of ways that you can have a positive impact and help you get up to speed.

## Ways to Contribute

A programming language is a large undertaking, and we need many different kinds of help from many different kinds of people. Even if you’re not experienced with programming language implementation, Haskell, or concatenative programming, you can still make a difference for everyone who comes after you.

Follow the directions in the [README] to build the Kitten compiler, then try out the interactive interpreter by running `kitten`, or the batch interpreter by running `kitten filename.ktn`. I encourage you to try developing small programs, while keeping track of problems you encounter. It’s a new programming language, so I won’t sugar-coat it: you should be prepared to run into bugs. However, every problem we find and fix is a frustration that the next person doesn’t have to suffer.

As you go along, feel free to submit [issues] or pull requests. Here are some examples of areas I’d like to hear about:

 * **Documentation** — If you find errors or incomplete information in Kitten’s documentation, such as the [README], [examples], and [wiki]. For issues about [kittenlang.org], head over to the [kittenlang.org repo] and file an issue there. You can build the internal compiler documentation with `stack haddock`; I’d appreciate patches to keep this documentation up to date and correctly formatted.

 * **Error Messages** — If you encounter unclear or misleading error messages, especially if they point to weird source locations. Bad error messages are considered bugs; the goal is to have the compiler help you *learn* the language as you use it.

 * **Examples** — If you’ve written a short, self-contained program in Kitten that you think would make a good example to demonstrate the language, such as a [Rosetta Code] solution; or if you find an error in the existing examples.

 * **Tooling** — If you find the tooling awkward or incomplete in any way, or you have an idea for a nice usability improvement, such as an interactive mode command, type system feature, or syntax change.

 * **Standard Library** — If you find yourself missing a useful utility function that you think should go in the [common vocabulary].

 * **Performance** — If you encounter poor performance in the form of excessive run time, memory usage, or compile time, particularly with simple code that you would expect to be fast.

 * **Design** — If you have suggestions for improving the visual design of Kitten resources such as [kittenlang.org].

 * **Accessibility** — If you encounter accessibility issues with the tooling or documentation, for example as a person with a visual impairment or limited mobility.

Contributors should agree to abide by the guidelines for respectful collaboration set out in the [Code of Conduct].

## Style Guidelines

As a rule of thumb, when contributing Haskell or Kitten code, try to follow the style of the surrounding code, unless it sucks, in which case defer to your best judgement.

The Haskell code uses 2-space indents and (usually) an 80-column limit, breaks lines before infix operators and after layout keywords such as `do` & `let`, rarely aligns things across lines, and prefers explicit or qualified imports. Otherwise, it generally follows [Johan Tibell’s Haskell Style Guide](https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md), such as using `UpperCamelCase` for type and constructor names and `lowerCamelCase` names for functions and variables. It uses Haddock for internal documentation.

The Kitten code also uses 2-space indents and an 80-column limit, with layout (`:`) for multi-line blocks and brackets `{}` for single-line blocks. By convention, type names and type variables are `UpperCamelCase`, while function names and local variables are `lower_underscore_case`. Documentation in Kitten code should be written in the `docs` section of metadata (`about`) blocks using Markdown formatting.

Both the Haskell and Kitten code lean toward point-free style where reasonable.

[Code of Conduct]: https://github.com/evincarofautumn/kitten/blob/master/CODE_OF_CONDUCT.md
[README]: https://github.com/evincarofautumn/kitten/blob/master/README.md
[Rosetta Code]: http://rosettacode.org/
[common vocabulary]: https://github.com/evincarofautumn/kitten/blob/master/common.ktn
[examples]: https://github.com/evincarofautumn/kitten/tree/master/examples
[issues]: https://github.com/evincarofautumn/kitten/issues
[kittenlang.org repo]: https://github.com/evincarofautumn/kittenlang.org
[kittenlang.org]: http://kittenlang.org
[wiki]: https://github.com/evincarofautumn/kitten/wiki

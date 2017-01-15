# Architecture

Kitten’s compiler has a fairly simple architecture, with some features inspired by Forth.

The compiler acts as a kind of database server for a `Dictionary`, which maps a fully `Qualified`, fully `Instantiated` name to an `Entry` containing information about a single program element, such as a word, trait, synonym, or type. A request consists of attempting to `Enter` a program `Fragment` into the `Dictionary`. A `Fragment` usually comes from a source file or interactive input, and consists of a set of top-level declarations and definitions. This request may fail in the `K` monad, which can log a `Report` because it’s an `Informer`.

To parse a `Fragment` from source, we `Tokenize` the input into a `Token` stream, where each token is `Located` at some `Origin` in the source. Here, we also insert curly brace tokens to desugar `Layout`-based syntax. Then we `Parse` each top-level program `Element` from the tokens, and collect them into a `Fragment`.

When entering a `Fragment` into a `Dictionary`, we treat the `Fragment` as a series of smaller requests: forward-declaring, resolving names, and entering definitions. To enter a `Definition`, we `Resolve` the names in the `Definition` against the `Dictionary`, from `Unqualified` or relative `Qualified` names into absolute `Qualified` names and local indices. That allows us to desugar `Infix` operators into postfix function calls, and to resolve the `Scope` of local and closure variables, respectively. Then we `Infer` and check the type of the body, and finally lift `Quotations` into separate definitions.

Currently, you can `Interpret` a `Dictionary`, and the `Interactive` mode provides a UI for this. Different compilation targets (ELF, PE, Mach-O; x86, x86-64, ARM) and source-processing tools (documentation generator, CTAGS generator, auto-formatter, syntax highlighter) will consist of different serialisations of the `Dictionary`.

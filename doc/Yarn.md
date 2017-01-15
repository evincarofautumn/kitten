> **Note:** this document applies to an older version of the compiler; this IR is no longer used.

# Yarn

Yarn is the name for Kitten’s virtual machine and the assembly language for that machine. It is a low-level assembly language designed to be produced and consumed primarily by machines, not humans.

# Execution Model

Yarn is a stack-based runtime with four conceptual stacks. In an actual implementation, some of these might be merged, stored in registers, or optimized away.

<table>

  <thead>
    <tr>
      <th>Stack Name</th>
      <th>Description</th>
    </tr>
  </thead>

  <tbody>
    <tr>
      <td><code>Data</code></td>
      <td>Stores the parameters and return values of functions.</td>
    </tr>

    <tr>
      <td><code>Local</code></td>
      <td>Stores local variables moved from the data stack.</td>
    </tr>

    <tr>
      <td><code>Closure</code></td>
      <td>Stores local values captured by function activations.</td>
    </tr>

    <tr>
      <td><code>Return</code></td>
      <td>Stores the return address for a function call.</td>
    </tr>
  </tbody>

</table>

# Instruction Set

By convention, instructions produced from the AST are capitalized while builtins are lowercase.

## Labels

### `Label LABEL`

Creates a label named `LABEL`, an unsigned integer.

## Pushing

### `Push HINT VALUE`

Pushes a scalar value to `Data`. The format of `VALUE` depends on the type hint `HINT`:

<table>

  <thead>
    <tr>
      <th><code>HINT</code></th>
      <th><code>VALUE</code></th>
    </tr>
  </thead>

  <tbody>
    <tr>
      <td><code>Bool</code></td>
      <td><code>0</code> or <code>1</code></td>
    </tr>

    <tr>
      <td><code>Char</code></td>
      <td>UTF-32 code point as decimal integer</td>
    </tr>

    <tr>
      <td><code>Int</code></td>
      <td>Signed 64-bit integer</td>
    </tr>

    <tr>
      <td><code>Float</code></td>
      <td>Double-precision floating-point number</td>
    </tr>

    <tr>
      <td><code>Label</code></td>
      <td>Label (function pointer)</td>
    </tr>

    <tr>
      <td><code>Nil</code></td>
      <td><code>0</code></td>
    </tr>
  </tbody>

</table>

## Tuples

### `Pair`

Wraps the top two elements of `Data` in a cons pair.

## Vectors

### `Vec SIZE`

Wraps the top `SIZE` elements of `Data` in a new vector, where `SIZE` is an unsigned integer.

## Calling and Branching

### `Branch LABEL`

Pops a value from `Data`. If the value is `false`, jumps to `LABEL`.

### `Call LABEL`

Saves the return address on `Return` and jumps to `LABEL`.

### `Jump LABEL`

Jumps unconditionally to `LABEL`.

### `Ret`

Pops an address from `Return` and jumps to it.

## Locals and Closures

### `Act LABEL VALUE*`

Pushes a function activation to `Data` consisting of a `LABEL` and zero or more values addressed in the format `STACK:INDEX`, where `STACK` is either `Local` or `Closure` and `INDEX` is an offset onto that stack. For example:

```
Act 42 Local:0 Closure:1
```

This creates an activation addressing label `42`, whose closure contains value `0` from `Local` and value `1` from `Closure`.

### `Closure INDEX`

Copies the value at `INDEX` in `Closure` to `Data`.

### `Enter`

Pops a value from `Data` and pushes it to `Local`.

### `Leave`

Drops the top value from `Local`.

### `Local INDEX`

Copies the value at `INDEX` in `Local` to `Data`.

## Builtins

This list is subject to change. Many instructions are of the form `NAME HINT` where `HINT` is the type of operand to expect and/or result to produce, both on `Data`. The prelude contains type signatures for all of these.

### Arithmetic

 * `add float`
 * `add int`
 * `add vector` (Concatenation)
 * `div float`
 * `div int`
 * `mod float`
 * `mod int`
 * `mul float`
 * `mul int`
 * `neg float`
 * `neg int`
 * `sub float`
 * `sub int`

### Relational

Vectors are lexicographically ordered, starting from the topmost element.

 * `eq char`
 * `eq float`
 * `eq int`
 * `eq vector`
 * `ge char`
 * `ge float`
 * `ge int`
 * `ge vector`
 * `gt char`
 * `gt float`
 * `gt int`
 * `gt vector`
 * `le char`
 * `le float`
 * `le int`
 * `le vector`
 * `lt char`
 * `lt float`
 * `lt int`
 * `lt vector`
 * `ne char`
 * `ne float`
 * `ne int`
 * `ne vector`

### Logical and Bitwise

 * `and bool`
 * `and int`
 * `not bool`
 * `not int`
 * `or bool`
 * `or int`
 * `xor bool`
 * `xor int`

### Increment and Decrement

 * `dec float`
 * `dec int`
 * `inc float`
 * `inc int`

### Vectors

 * `bottom`—`[x, _, _, _]`
 * `down`—`[x, x, x, _]`
 * `empty`
 * `get`
 * `length`
 * `set`
 * `top`—`[_, _, _, x]`
 * `up`—`[_, x, x, x]`
 * `vector`

### Functions

 * `apply`
 * `compose`
 * `function`

### Stack

 * `drop`
 * `dup`
 * `swap`

### Tuples

 * `first`
 * `rest`

### I/O

 * `close`
 * `get_line`
 * `open_in`
 * `open_out`
 * `print`
 * `show float`
 * `show int`
 * `stderr`
 * `stdin`
 * `stdout`

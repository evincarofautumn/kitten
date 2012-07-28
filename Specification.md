# The Kitten Report

Being an in-progress specification for the Kitten programming language as it
will be.

## 1. Introduction

Kitten is a general-purpose impure functional programming language with
concatenative semantics. It is statically typed, with partial type inference and
full type checking in the presence of type annotations. The language provides a
small but powerful set of core features, including:

 * Extensible syntax through pattern matching and term rewriting

 * User-defined algebraic data types (ADTs)

 * Higher-order functions

 * Optional non-strict evaluation semantics

 * A module system

 * A “batteries included” standard library with common data structures such as
   lists, maps, and sets

The language has a rich set of built-in data types, including:

 * Unicode strings

 * Homogeneous and heterogeneous arrays

 * Fixed- and arbitrary-precision exact integers

 * Arbitrary-precision exact rational numbers

 * Fixed-precision inexact floating-point numbers

 * Complex numbers

Kitten is motivated by a desire for a powerful functional programming language
with high performance and minimal runtime support. As a concatenative language,
it is highly amenable to analysis, visualization, refactoring, and higher-order
programming, yet also admits a very efficient implementation. Its design is
based on programming language usability research, in the hope of providing a
pleasurable programming experience for beginners and experienced users alike.

The language is small, but not deliberately minimalistic. Whereas most
concatenative languages are very visibly stack-oriented, Kitten’s facilities for
pattern matching and infix syntax abstract away from the stack as an
implementation detail.

## 2. Lexical Structure

A Kitten program consists of zero or more whitespace-separated terms, being
numbers, quotations, words, symbols, or directives.

### 2.1. Whitespace

Whitespace may be literal whitespace or a comment. Literal whitespace is a
sequence of one or more Unicode whitespace characters. A comment may be
single-line or multi-line. A single-line comment begins with two hyphens `--`
(1) or an em dash `—` (2) and continues till the following line break (or
end-of-file).

    meow  -- Demand attention.
    meow? —  Ensure attention was received.

A multi-line comment begins with an opening curly brace followed by a hyphen
`{-` (3) or em dash `{—` and ends with a closing curly brace preceded by a
hyphen `-}` or em dash `—}` respectively (4). Multi-line comments may be
nested. The brace syntax was chosen to allow easily jumping between matching
braces in text editors, and to give a visual indication of comment nesting.

    {-
    This is a multi-line comment. It can contain any text, including sequences
    of hyphens (--) or braces {}, and ends at the matching comment-close.
    -}

    {-
    This is a comment.

    {-
    This is too. It will remain such even if the outer one is removed.
    -}

    This text belongs to the outer comment.
    -}

    {— Outer comment begins.
       {— Inner comment begins and ends. —}
       Outer comment ends. —}

##### Code Points

 1. `-` U+002D HYPHEN-MINUS

 2. `—` U+2014 EM DASH

 3. `{` U+007B LEFT CURLY BRACKET

 4. `}` U+007D RIGHT CURLY BRACKET

### 2.2. Numbers

A number is either an integer, a rational, an inexact, or a complex. Regardless
of its type, any number may be preceded by a sign character `+` (1), `-` (2), or
`−` (3).

##### Code Points

 1. `+` U+002B PLUS SIGN

 2. `-` U+002D HYPHEN-MINUS

 3. `−` U+2212 MINUS SIGN

#### 2.2.1. Integer Literals

Integers consist of one or more digits, followed by an optional radix specifier
consisting of a number sign `#` (1) and a decimal integer. The default radix is
decimal. Implementations must support at least decimal (`#10`), hexadecimal
(`#16`), octal (`#8`), and binary (`#2`) integer literals. Valid digits are a
subset of digit characters `0`–`9` (2) and letters dependent on the base—for
hexadecimal integers, `A`–`F` (3) and `a`–`f` (4).

##### Examples

    0
    42
    42#10
    deafbeef#16
    0777#8
    1010#2

##### Regular Expression

      [0-9]+       (#10)?
    | [0-9A-Fa-f]+ #16
    | [0-7]+       #8
    | [01]+        #2

##### Code Points

 1. `#` U+0023 NUMBER SIGN

 2. `0` U+0030 DIGIT ZERO – `9` U+0039 DIGIT NINE

 3. `A` U+0041 LATIN CAPITAL LETTER A – `F` U+0046 LATIN CAPITAL LETTER F

 4. `a` U+0061 LATIN SMALL LETTER A – `f` U+0066 LATIN SMALL LETTER F

##### Typesetting

Radix specifiers ought to be typeset as subscripts. Hexadecimal numbers should
be set in capitals, or small capitals when typesetting with text figures.

    deafbeef#16

> DEAFBEEF<sub>16</sub>

##### Notes

The goal of a numeric literal syntax is to be legible and make the radix
explicit, while remaining unambiguous with identifiers and other lexemes. The
conventional `0x` prefix for hexadecimal numbers is often read as “zero times”
by beginners, which is misleading.

A more mathematically oriented subscript syntax was considered (with `_16` and
`₁₆`). However, Unicode subscript digits are poor semantic substitutes for true
digit characters, and likewise poor visual substitutes for true subscripts. The
`#` sign is mnemonic, and much less likely to clash with desirable identifiers.

#### 2.2.2. Fraction Literals

A fraction literal consists of one or more digits, followed by a slash `/` (1)
or fraction slash `⁄` (2), one or more digits again, and an optional radix
specifier. Implementations must support at least decimal (`#10`), hexadecimal
(`#16`), octal (`#8`), and binary (`#2`) fraction literals.

##### Examples

    1/2
    2/3
    22/7
    101/010#2
    628318/10000
    8090A0/FFFFFF#16

##### Regular Expression

      [0-9]+       [/⁄] [0-9]+       (#10)?
    | [0-9A-Fa-f]+ [/⁄] [0-9A-Fa-f]+ #16
    | [0-7]+       [/⁄] [0-7]+       #8
    | [01]+        [/⁄] [01]+        #2

##### Code Points

 1. `/` U+002F SLASH

 2. `⁄` U+2044 FRACTION SLASH

##### Typesetting

The whole literal should be set as a proper fraction with the numerator above
the denominator. Radix specifiers ought to be typeset as subscripts following
*both* the numerator and denominator.

#### 2.2.3. Inexact Literals

Inexact literals consist of a significand and an optional exponent part.

The significand comprises one or more digits, followed by a decimal point `.`
(1), one or more digits again, and an optional radix specifier as previously
defined. Implementations must support at least decimal (`#10`), hexadecimal
(`#16`), octal (`#8`), and binary (`#2`) inexact literals.

The exponent part comprises an asterisk `*` (2) or multiplication sign `×` (3),
one or more decimal digits constituting a base specifier, a caret `^` (4), an
optional sign character `+`, `-`, or `−`, and the exponent proper of one or more
decimal digits. Implementations must support at least `10`, `16`, `8`, and `2`
as base specifiers.

##### Examples

    1.0
    0.25
    6.28318

    10.5
    A.8#16
    12.4#8
    1010.1#2

    1.0*2^16

    −1.0×10^−5    -- −1/100000
    A.0×16^2      -- A00.0#16
    +10.0#8*10^+3 -- 8000
    0.0628*10^2   -- 6.28

##### Regular Expression

    ( [0-9]+       \. [0-9]+      (#10)?
    | [0-9A-Fa-f]+ \. [0-9A-Fa-f] #16
    | [0-7]+       \. [0-7]+      #8
    | [01]+        \. [01]+       #2
    )
    ( \* (10|16|8|2) \^ [-+−]? [0-9]+ )?

##### Code Points

 1. `.` U+002E PERIOD

 2. `*` U+002A ASTERISK

 3. `×` U+00D7 MULTIPLICATION SIGN

 4. `^` U+005E CARET

##### Typesetting

Radix specifiers ought to be typeset as subscripts, and exponents as
superscripts.

    +10.0#8*10^+3

> +10.0<sub>8</sub> × 10<sup>+3</sup>

##### Notes

Exponents in alternate bases were considered and deemed not widely applicable
enough for inclusion. The characters `e` and `E` were considered for exponent
specifiers (as a shorthand for `*10^`), but this syntax is not widely known
outside the scientific and computing communities, and does not generalise to
bases other than decimal. In this vein, `ᴇ` (U+1D07 LATIN LETTER SMALL CAPITAL
E) was considered as well, but Unicode small capital characters are poor
substitutes for proper typographical small capitals. Similarly, Unicode
superscript digits are poor substitutes for proper typographical superscripts,
while `^` is already a common convention for superscripts.

#### 2.2.4. Complex Literals

Two number literals of any kind may be joined by a `+`, `-`, or `−`, and
suffixed with the imaginary unit `i` (1) or `j` (2). The result is a complex
literal. When the literal is prefixed with a sign character, it applies only to
the real part, so `-2+3i` has a real part of `-2` and an imaginary part of
`+3i`.

##### Examples

    2+2i
    1/2−1/4j

##### Code Points

 * `i` U+0069 LATIN SMALL LETTER I

 * `j` U+006A LATIN SMALL LETTER J

### 2.3. Quotations

Quotations are the fundamental data structure of Kitten. They are used to store
homogeneous vectors of unboxed values, heterogeneous vectors of boxed values,
and anonymous functions. They are used extensively in the construction of other
data structures, such as lists, maps, sets, and trees.

#### 2.3.1. Plain Quotations

Plain quotations consist of zero or more whitespace-separated terms surrounded
by square brackets `[]` (1, 2). Terms within quotations are not evaluated, so a
quotation can be used to construct anonymous functions.

##### Code Points

 1. `[` U+005B LEFT SQUARE BRACKET

 2. `]` U+005C RIGHT SQUARE BRACKET

#### 2.3.2. Text and Code Point Quotations

Text quotations consist of a series of zero or more character literals and
character escapes wrapped in double quotes—either straight double quotes `""`
(1) or curved double quotes `“”` (2). Each value in a text quotation is a
Unicode code point; a single code point may be specified literally using a code
point quotation wrapped in straight single quotes `''` (3) or curved single
quotes `‘’` (4). Thus a text quotation is syntactic sugar for a plain quotation
of code point quotations.

##### Examples

    "meow"
    ['m' 'e' 'o' 'w']

##### Code Points

 1. `"` U+0022 QUOTATION MARK

 2. `“` U+201C LEFT DOUBLE QUOTATION MARK – `”` U+201D RIGHT DOUBLE QUOTATION MARK

 3. `'` U+0027 APOSTROPHE

 4. `‘` U+2018 LEFT SINGLE QUOTATION MARK – `’` U+2019 RIGHT SINGLE QUOTATION MARK

####. 2.3.3 Character Literals and Character Escapes

A character literal is any Unicode character except for backslash `\`
(U+005C). Line breaks are allowed within text quotations. Curved quotes may be
nested, so a text quotation using curved double quotes may itself contain curved
double quote character literals, provided they are correctly matched. One using
straight quotes, however, would not allow literal straight quotes. A character
escape consists of a backslash `\` followed by a *character escape body*, which
is translated to a particular character sequence:

 * `\\` → `\` U+005C BACKSLASH

 * `\'` → `'` U+0027 APOSTROPHE‡

 * `\"` → `"` U+0022 QUOTATION MARK‡

 * `\a` = `\BEL` → U+0007 BELL

 * `\b` = `\BS` → U+0008 BACKSPACE

 * `\e` = `\ESC` → U+001B ESCAPE

 * `\f` = `\FF` → U+000C FORM FEED

 * `\n` = `\LF` → U+000A LINE FEED

 * `\r` = `\CR` → U+000D CARRIAGE RETURN

 * `\s` = `\SPC` → U+0020 SPACE

 * `\t` = `\TAB` = `\HT` → U+0009 TAB

 * `\v` = `\VT` → U+000B VERTICAL TAB

 * `\XXXXXX` → U+XXXXXX†

 * `\{XXXXXX}` → U+XXXXXX†

 * ASCII escapes

    * `\NUL` → U+0000 NULL

    * `\SOH` → U+0001 START OF HEADING

    * `\STX` → U+0002 START OF TEXT

    * `\ETX` → U+0003 END OF TEXT

    * `\EOT` → U+0004 END OF TRANSMISSION

    * `\ENQ` → U+0005 ENQUIRY

    * `\ACK` → U+0006 ACKNOWLEDGE

    * `\BEL` → U+0007 BELL

    * `\BS` → U+0008 BACKSPACE

    * `\HT` → U+0009 TAB

    * `\LF` → U+000A LINE FEED

    * `\VT` → U+000B VERTICAL TAB

    * `\FF` → U+000C FORM FEED

    * `\CR` → U+000D CARRIAGE RETURN

    * `\SO` → U+000E SHIFT OUT

    * `\SI` → U+000F SHIFT IN

    * `\DLE` → U+0010 DATA LINK ESCAPE

    * `\DC1` → U+0011 DEVICE CONTROL 1

    * `\DC2` → U+0012 DEVICE CONTROL 2

    * `\DC3` → U+0013 DEVICE CONTROL 3

    * `\DC4` → U+0014 DEVICE CONTROL 4

    * `\NAK` → U+0015 NEGATIVE ACKNOWLEDGE

    * `\SYN` → U+0016 SYNCHRONOUS IDLE

    * `\ETB` → U+0017 END OF TRANSMISSION BLOCK

    * `\CAN` → U+0018 CANCEL

    * `\EM` → U+0019 END OF MEDIUM

    * `\SUB` → U+001A SUBSTITUTION

    * `\ESC` → U+001B ESCAPE

    * `\FS` → U+001C FILE SEPARATOR

    * `\GS` → U+001D GROUP SEPARATOR

    * `\RS` → U+001E RECORD SEPARATOR

    * `\US` → U+001F UNIT SEPARATOR

    * `\SP` → ` ` U+0020 SPACE

    * `\DEL` → U+007F DELETE

† The `\XXXXXX` escapes accept between one and six hexadecimal digits, and
produce the corresponding Unicode code point. The version wrapped in curly
braces `{}` prevents trailing hexadecimal digits from being included in the code
point value. As a special case, the escape `\{}` is valid and translates to no
characters whatsoever—it is illegal in a code point literal. This allows
`"\SO\{}H"`—Shift Out followed by capital H—to be differentiated from
`"\SOH"`—just Start of Heading. It also allows proper indentation of multi-line
text quotations:

‡ As a matter of style, compilers ought to warn about unnecessary escaping of
quote characters within text quotations.

#### 2.3.4. Heredocs

The escape `\<WORD>` indicates the presence of a *here document*, abbreviated
*heredoc*. A heredoc is a literal text quotation, which begins on the line
following the text quotation and ends when the `WORD` appears on a line by
itself.

    some_c_code => "// Here is my awesome program:\n\n\<END>"
    #include <stdio.h>
    int main(int argc, char** argv) {
        printf("Hello, world!\n");
        return 0;
    }
    END

If the ending delimiter is indented, then all whitespace preceding its column
position will be discarded. This allows for heredocs to be properly indented
with the surrounding code.

    my_paragraph => "\<END>"
      'Twas brillig, and the slithy toves
      did gyre and gimble in the wabe.
      All mimsy were the borogoves,
      and the mome raths outgrabe.
      END

The body of a heredoc is spliced into the text quotation at the point where it
appeared. Multiple here documents may be referenced in a single line, in which
case they are parsed in the order mentioned:

    paragraphs => ["\<END-FIRST>\n----\n\<END-SECOND>" "\<END-THIRD>"]
    This is the first.
    END-FIRST
    This is the second.
    END-SECOND
    This is the third.
    END-THIRD

##### Notes

It was considered to allow heredocs with blank identifiers, i.e., `\<>`, such
that the heredoc would end at the first blank line. This was deemed excessively
error-prone. It would also be inconsistent with the treatment of indentation for
regular heredocs, as it would require a line consisting of only whitespace
characters—such lines are silently erased by many editors.

#### 2.3.5. Quasiquotation

Quasiquotation is syntactic sugar for splicing the results of expressions into a
quotation. A quasiquotation begins with a backtick <code>\`</code> (1) followed
by a quotation, into which quotations preceded by a backslash `\` are
automatically spliced. For example:

    `["Hello, " \[name [to-upper] map] "!"]
    concat output-line

The above is exactly equivalent to the following expression:

    ["Hello, "]
    name [to-upper] map quote compose
    ["!"] compose
    concat output-line

Splice expressions may use applicative syntax with parentheses `()` instead of
square brackets (see §2.7):

   `["In five years you will be " \(show (age + 5)) " years old."]
   concat output-line

Splices may also appear within text quotations, where `show` is automatically
called on the result of the splice.

    "In five years you will be \(age + 5) years old." output-line

An empty splice expression (i.e., `\[]` or `\()`) in a quasiquotation yields a
function which, when evaluated on the correct number of arguments, produces the
concatenated quotation. There is no distinction between square brackets and
parentheses in this case.

    intro => "Her name was \() and his was \[]."
    "Alice" "Bob" intro

For named splices, you can use inline definitions. Then the order need not be
fixed:

    intro => [[hers his] => "His name was \[his] and hers was \(hers)."]
    "Alice" "Bob" intro

 1. <code>\`</code> U+0060 GRAVE ACCENT

 2. `\` U+005C BACKSLASH

### 2.4. Words

A word consists of a letter or symbol followed by zero or more letters, digits,
and symbols. A letter is any Unicode letter, and a symbol is any Unicode
punctuation except for the characters `:"“”'‘’[](){}\#.`.

Words are case-sensitive and do not undergo Unicode normalization.

##### Examples

    meow
    Purr
    read-file
    empty?
    >=
    ≥
    →
    ++

### 2.5. Symbols

A symbol is a word preceded by a dot `.`.

##### Examples

    .new
    .green
    .→

### 2.6. Special Syntactic Forms

#### 2.6.1. Definitions and Inline Definitions

The definition directive `=>` (1, 2) or `⇒` (3) binds a pattern to a
substitution:

    pattern => substitution
    pattern ⇒ substitution

A pattern is a quotation consisting of one or more pattern terms. A pattern term
is a symbol, a quotation pattern, an ADT pattern, a scalar variable, a row
variable, or a literal. A quotation pattern has the same syntax as a whole
pattern. An ADT pattern matches ADT quotations of a particular tag, but its
contents are identical in form to a quotation pattern. A scalar variable is a
word. A row variable is a word preceded by `...` or `…` (4).

The top (last) symbol in a top-level definition’s pattern must be a word, which
is treated as the name of the definition. Inline definitions inside a quotation,
however, are anonymous.

    -- Top-level definition of "not" function.
    [True not] => [False]
    [False not] => [True]

    -- Inline definition of "not" function bound to a top-level word.
    [not] =>
    [ [True] => [False]
      [False] => [True] ]

    -- Inline pattern matching with an anonymous definition.
    kitty-size
    [ LittleKitty => "mew"  BigKitty => "MAO" ] apply
    output-line

Brackets may be omitted around a pattern or definition when it consists of a
single term. Implementations ought to warn about redundant brackets.

    not =>
    [ True => False
      False => True ]

As with top-level bindings, this is rewritten internally to use explicit
equality testing with `=`.

    [not] =>
    [ [[dup True =] [drop False]]
      [[dup False =] [drop True]]
      cond ]

##### Notes

The original syntax for definitions used a `define` keyword, i.e.:

    define [not]
    [ define [True] [False]
      define [False] [True] ]

However, this was considered too verbose, especially for the case of inline
definitions.

##### Typesetting

`=>` should be typeset as a rightward double arrow.

    not => [ True => False  False => True ]

> *not* ⇒ [ *True* ⇒ *False* &emsp; *False* ⇒ *True* ]

#### 2.6.2. Operators

    infix-left  precedence word
    infix-right precedence word
    prefix      precedence word
    postfix     precedence word

The `infix-left` directive declares that a word may be used as a
left-associative infix operator with a particular precedence, a decimal integer
from 0 (loosest) to 10 (tightest). The `infix-right` directive works similarly
for right-associative operators, and the pattern holds for the `prefix` and
`postfix` directives. Operator precedence and associativity declarations have no
effect in normal concatenative mode; they are only relevant to applicative
syntax.

##### Examples

    prefix     9 ~
    infix-left 8 &
    infix-left 7 |

    (~a | b & ~c = ((~a) | (b & (~c))))

#### 2.6.3. Optimization Hints

 * `commutative n f` declares that order is immaterial for the first `n`
   arguments of `f`. For example, `commutative 2 f` states that `x y f` is
   equivalent to `y x f`.

 * `distributive f g` declares that `f` is distributive over `g`, which is to
   say that `x f y f g` or `x y [f] biapply g` can be factored into `x y g f`.

 * `associative f` declares that `x y f z f` is equivalent to `x y z f f`.

 * `transitive f` declares that if `x y f True =` and `y z f True =` then `x z f
   True =` as well. Using `transitive` on non-predicates is not recommended and
   should trigger a warning.

Each of these directives may be applied to a name, or to only a particular
definition. For example, multiplication of integers is `commutative` while
matrix multiplication is not.

#### 2.6.4. Metadata

Any term can be annotated with arbitrary metadata. This can be used for
documentation, debugging, automated tests, or input methods for names with
unconventional symbols.

#### 2.6.5. Layout Quotations

An explicit quotation may always be replaced with a layout quotation. A layout
quotation consists of a colon `:` and a line break, followed by a region in
which each line is indented more than the first lexeme of the line where the
colon appeared.

    define [ x y z  f  triapply ]
    [ x f  y f  z f ]

    define [ x y z  f  triapply ]:
      x f
      y f
      z f

    [ "alpha"
      "beta" ]
    [ "gamma"
      "delta" ]
    compose

    :
      "alpha"
      "beta"
    :
      "gamma"
      "delta"
    compose

A layout region may begin on the same line as the colon, in which case
subsequent lines must only have greater indentation than the colon:

    : "alpha"
      "beta"
    : "gamma"
      "delta"
    compose

Layout blocks may be nested.

    define 3x3-identity-matrix
    [ [ 1 0 0 ]
      [ 0 1 0 ]
      [ 0 0 1 ] ]

    =

    define 3x3-identity-matrix
    :: 1 0 0
     : 0 1 0
     : 0 0 1

A kind of double-layout block was considered, which would begin with a
double-colon `::` and produce a nested quotation by keeping track of line
breaks. This was rejected on the basis of poor usability, because it would
introduce a non-obvious distinction between `::` and `: :`.

Layout syntax may be used anywhere, including within patterns or other quotations.

    -- 2x2 identity matrix multiplication.
    : :: 1 0
       : 0 1
      :: a b
       : c d
      *
    =>
    :: a b
     : c d

Layout quotations may be quasiquoted (see §2.3.5):

    `: \(n * 2)
       \(n * 3)
       \(n * 4)

##### Code Points

 * `:` U+003A COLON

### 2.7. Applicative Syntax

Parentheses `()` enclose an expression in applicative (as opposed to
concatenative) syntax. In applicative syntax, the order of function calls is
reversed, and function application is used in place of function composition.

    x y f   -- Concatenative syntax.
    (f y x) -- Applicative syntax.

Words that have been declared as operators function as such in applicative mode.

    infixl 4 +
    (a + b + c)

To treat an operator as an ordinary function in applicative mode, wrap it in
parentheses.

    ((+) a b)

Whereas partial function application is explicit in concatenative mode, it’s
implicit in applicative mode.

    [1 +] apply
    ((+) 1)

There is an alternative syntax for partial application of an operator, called a
*section*.

    ((+ 1))  -- [1 +]
    ((1 +))  -- [1 swap +]

Within applicative mode, parentheses are used for grouping of expressions.

    (2 * (3 + 5))

Square brackets return to concatenative mode.

    ([x dup *] + [y dup *])

To include a quotation, use double square brackets: the first to switch to
normal concatenative syntax, and the second to enclose the quotation.

    (each [[1 2 3]] * each [[4 5 6]])

## 3. Type System

Kitten is statically typed. The basic types are:

 * `Integer`

   A signed exact arbitrary-precision integer. Implementations must use native
   fixed-precision integers when possible.

 * `Fraction`

   A signed exact arbitrary-precision rational, that is, a quotient of two
   `Integer` values. Implementations must use native fixed-point values when
   possible, so this is not typically as large as two arbitrary-precision
   `Integer` values.

 * `Inexact`

   A signed inexact fixed-precision floating-point number. Implementations are
   expected to use a 64-bit representation by default, unless none is available,
   or another size is explicitly requested.

 * `Symbol`

   An interned string, whose only defined operation is equality. Symbols have no
   predefined ordering relation, but it is legal (if perhaps inadvisable) to
   define one.

 * `N T Quotation`

   A statically sized, homogeneous quotation with `N` elements of type `T`,
   i.e., an array.

 * `? T Quotation`

   A dynamically sized, homogeneous quotation with elements of type `T`, i.e., a
   vector.

 * `N ? Quotation`

   A statically sized, heterogeneous quotation with `N` elements, e.g., an
   anonymous function.

 * `? ? Quotation`

   A dynamically sized, heterogeneous quotation.

Side-effectful and pure quotations are differentiated.

Internally, Kitten’s type inference engine also makes use of union types and
intersection types. Union types describe values that belong to either of two
types, whereas intersection types describe values (typically functions) that
belong to both of two types.

Homogeneous quotations are more efficient and amenable to static analysis and
optimizations than their heterogeneous kin. In particular, SIMD instructions
love packed homogeneous vectors of fixed-size machine integers and floats.
Nested quotations can be packed as well, if all the inner quotations have the
same size and element type.

Five of Kitten’s six core primitives (`_dup`, `_swap`, `_drop`, `_quote`, and
`_apply`) and all of its arithmetic primitives (`_addi`, `_subi`, etc.), have
inferable static types. The last primitive, `_compose`, is inferable for
homogeneous quotations (a majority of data structures):

> ∀abcde. a × b c × d e → a × (b ∨ d) (c + e)

`_compose` is not inferable in the general case because it has a rank-2 type:

> ∀abcd. a × (∀e. e × a → e × b) × (∀f. f × b → f × c) → a × (∀g. g × a → g × c)

Therefore, functions using `_compose` on heterogeneous quotations must have an
explicit type signature.

Declaration syntax mirrors definition syntax:

    [consumption word] >> [production]

Declarations using C types are external, and automatically marshalled to and fro
(insofar as this is possible).

    [int int int uint32 SDL_SetVideoMode] >> pointer

Because Kitten words cannot have dynamic stack effects, variadic foreign
functions accept quotations instead:

    [... pointer printf] >> int
    ["text" 32] "%s %i" printf

## 4. Built-in Definitions

## 5. Standard Prelude

## 6. Object Model

An object is a function encapsulating a message table, which associates message
selectors with message handlers. A class is an object accepting a `.new`
message, which produces an instance of that class.

## 7. Modules

`module`, `import`, and `export` directives.

Standard module hierarchy:

 * `Control`

    * `Exception` (see §8)

 * `Data`

    * `List`

    * `Object`

 * `Kitten`

    * `Compiler`

## 8. Exceptions

Any computation may result in an *exceptional value* rather than an ordinary
value. Using a built-in word with invalid arguments, for example, results in an
exception. Exceptions interrupt the normal flow of evaluation, preventing any
code from executing until they are explicitly handled with the `catch`
built-in. `catch` accepts a function to which the exceptional value is passed as
an ordinary value. If the function’s patterns do not match the exception, it
continues to propagate until a suitable handler is found.

    1 0 /
    "This is never executed." output-line
    [[message >> ArgumentError] => ["Error: \[message]" output-line]] catch

The `throw` built-in word converts a value into an exceptional value, allowing
you to throw your own exceptions.

    [a b subtract-naturals] =>
    : ["Natural subtraction \[a] - \[b] = \[] < 0!" throw]
      a b - dup 0 < when

When throwing exceptions from within word definitions, it is considered good
practice to ensure that no arguments or other intermediate values remain on the
stack. Throwing ADTs is also preferable to throwing plain values or objects.

Exceptional values are used to indicate errors or other unusual conditions that
cannot or should not be handled within the scope from which they are thrown.
They are not first-class: you cannot do anything with an exception but handle it
with `catch`.

Standard exception hierarchy:

 * `Control` Standard control flow module; see §7.

    * `Exception` Standard exceptions.

       * `External` Exceptions from external sources (e.g., I/O errors).

          * `Overflow` Thrown when a value would exceed its maximum.

          * `Underflow` Thrown when a value would deceed its minimum.

          * `Domain` Thrown when a value is not in the domain of a function.

          * `Range` Thrown when a value is not in the range of a function.

       * `Internal` Exceptions from internal sources (e.g., failed assertions,
         internal errors).

          * `Overflow`

          * `Underflow`

          * `Domain`

          * `Range`

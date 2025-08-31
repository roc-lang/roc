# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module P]F
~~~
# TOKENS
~~~text
KwModule UpperIdent CloseSquare UpperIdent ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (uc "P")

    (malformed malformed:expr_unexpected_token)

    (uc "F")
))
~~~
# FORMATTED
~~~roc
module [P, ], F]

P
]
F
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **header_expected_open_square**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_011.md:1:1:1:8:**
```roc
module P]F
```
^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **]** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_011.md:1:9:1:10:**
```roc
module P]F
```
        ^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_011.md:1:8:1:9:**
```roc
module P]F
```
       ^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_011.md:1:9:1:10:**
```roc
module P]F
```
        ^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_011.md:1:10:1:11:**
```roc
module P]F
```
         ^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~

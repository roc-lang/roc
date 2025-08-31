# META
~~~ini
description=Example of importing constructors with wildcard from a nominal tag union
type=file
~~~
# SOURCE
~~~roc
module [red, green, blue]

import Color.*

red : Color
red = Red

blue : Color
blue = Blue

green : Color
green = Green
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent Comma LowerIdent Comma LowerIdent CloseSquare BlankLine KwImport UpperIdent Dot OpStar BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign UpperIdent BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign UpperIdent BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign UpperIdent ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "red")

    (lc "green")

    (lc "blue")
))
~~~
# FORMATTED
~~~roc
module [red, green, blue]


import Color
*

red : Color
red = Red
blue : Color
blue = Blue
green : Color
green = Green
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token ***

** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**nominal_import_wildcard.md:3:14:5:1:**
```roc
import Color.*

red : Color
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**nominal_import_wildcard.md:3:14:5:1:**
```roc
import Color.*

red : Color
```


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
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

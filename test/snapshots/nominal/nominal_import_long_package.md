# META
~~~ini
description=Example of importing a nominal tag union from a module within a package, and renaming it using `as`
type=file
~~~
# SOURCE
~~~roc
module [red]

import design.Styles.Color exposing [Encoder as CE]

red : CE
red = ... # not implemented
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare BlankLine KwImport LowerIdent Dot UpperIdent Dot UpperIdent KwExposing OpenSquare UpperIdent KwAs UpperIdent CloseSquare BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign TripleDot LineComment ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "red")
))
~~~
# FORMATTED
~~~roc
module [red]


import design.Styles | Color exposing [Encoder]
as 
CE
]

red : CE
red = ...# not implemented
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **as ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**nominal_import_long_package.md:3:46:3:49:**
```roc
import design.Styles.Color exposing [Encoder as CE]
```
                                             ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **]

** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**nominal_import_long_package.md:3:51:5:1:**
```roc
import design.Styles.Color exposing [Encoder as CE]

red : CE
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**nominal_import_long_package.md:3:46:3:49:**
```roc
import design.Styles.Color exposing [Encoder as CE]
```
                                             ^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**nominal_import_long_package.md:3:49:3:51:**
```roc
import design.Styles.Color exposing [Encoder as CE]
```
                                                ^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**nominal_import_long_package.md:3:51:5:1:**
```roc
import design.Styles.Color exposing [Encoder as CE]

red : CE
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
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~

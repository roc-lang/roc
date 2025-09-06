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
PARSE ERROR - nominal_import_long_package.md:3:21:3:27
PARSE ERROR - nominal_import_long_package.md:3:28:3:36
PARSE ERROR - nominal_import_long_package.md:3:37:3:38
PARSE ERROR - nominal_import_long_package.md:3:46:3:48
PARSE ERROR - nominal_import_long_package.md:3:51:3:52
MODULE NOT FOUND - nominal_import_long_package.md:3:1:3:21
UNDECLARED TYPE - nominal_import_long_package.md:5:7:5:9
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
  (Stmt.import)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.type_anno
    (name "red")
    (type uc)
  )
  (Stmt.assign
    (pattern (Patt.ident "red"))
    (Expr.malformed)
  )
)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
~~~

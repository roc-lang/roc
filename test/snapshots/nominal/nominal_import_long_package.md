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
(block
  (import
    (binop_exposing
      (binop_pipe
        (binop_pipe
          (lc "design")
          (uc "Styles")
        )
        (uc "Color")
      )
      (list_literal
        (uc "Encoder")
      )
    )
  )
  (malformed)
  (uc "CE")
  (malformed)
  (binop_colon
    (lc "red")
    (uc "CE")
  )
  (binop_equals
    (lc "red")
    (ellipsis)
  )
)
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


**UNDEFINED VARIABLE**
Nothing is named **design** in this scope.
Is there an **import** or **exposing** missing up-top?

**nominal_import_long_package.md:3:8:3:14:**
```roc
import design.Styles.Color exposing [Encoder as CE]
```
       ^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
  (Expr.malformed)
  (Expr.tag_no_args)
  (Expr.malformed)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "red"))
    (type type_15)
  )
  (Stmt.assign
    (pattern (Patt.ident "red"))
    (Expr.malformed)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 24
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 _)
(var #11 _)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 _)
(var #16 _)
(var #17 -> #23)
(var #18 _)
(var #19 _)
(var #20 _)
(var #21 _)
(var #22 _)
(var #23 _)
~~~
# TYPES
~~~roc
red : _a
~~~

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
(block
  (import
    (uc "Color")
  )
  (malformed)
  (binop_colon
    (lc "red")
    (uc "Color")
  )
  (binop_equals
    (lc "red")
    (uc "Red")
  )
  (binop_colon
    (lc "blue")
    (uc "Color")
  )
  (binop_equals
    (lc "blue")
    (uc "Blue")
  )
  (binop_colon
    (lc "green")
    (uc "Color")
  )
  (binop_equals
    (lc "green")
    (uc "Green")
  )
)
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
PARSE ERROR - nominal_import_wildcard.md:3:13:3:15
MODULE NOT FOUND - nominal_import_wildcard.md:3:1:3:13
UNDECLARED TYPE - nominal_import_wildcard.md:5:7:5:12
UNDECLARED TYPE - nominal_import_wildcard.md:8:8:8:13
UNDECLARED TYPE - nominal_import_wildcard.md:11:9:11:14
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


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
  (Expr.malformed)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "red"))
    (type type_8)
  )
  (Stmt.assign
    (pattern (Patt.ident "red"))
    (Expr.tag_no_args)
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "blue"))
    (type type_14)
  )
  (Stmt.assign
    (pattern (Patt.ident "blue"))
    (Expr.tag_no_args)
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "green"))
    (type type_20)
  )
  (Stmt.assign
    (pattern (Patt.ident "green"))
    (Expr.tag_no_args)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 27
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
(var #10 -> #11)
(var #11 _)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 _)
(var #16 -> #17)
(var #17 _)
(var #18 _)
(var #19 _)
(var #20 _)
(var #21 _)
(var #22 -> #23)
(var #23 _)
(var #24 _)
(var #25 _)
(var #26 _)
~~~
# TYPES
~~~roc
blue : _a
red : _a
green : _a
~~~

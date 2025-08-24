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
KwModule OpenSquare LowerIdent Comma LowerIdent Comma LowerIdent CloseSquare KwImport UpperIdent Dot OpStar LowerIdent OpColon UpperIdent LowerIdent OpAssign UpperIdent LowerIdent OpColon UpperIdent LowerIdent OpAssign UpperIdent LowerIdent OpColon UpperIdent LowerIdent OpAssign UpperIdent ~~~
# PARSE
~~~clojure
(block
  (import
    (uc "Color")
  )
  (malformed malformed:expr_unexpected_token)
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
NO CHANGE
~~~
# EXPECTED
PARSE ERROR - nominal_import_wildcard.md:3:13:3:15
MODULE NOT FOUND - nominal_import_wildcard.md:3:1:3:13
UNDECLARED TYPE - nominal_import_wildcard.md:5:7:5:12
UNDECLARED TYPE - nominal_import_wildcard.md:8:8:8:13
UNDECLARED TYPE - nominal_import_wildcard.md:11:9:11:14
# PROBLEMS
**Parse Error**
at 3:14 to 3:14

**Unsupported Node**
at 3:1 to 3:13

**Unsupported Node**
at 3:14 to 3:14

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "red")
    (Expr.apply_tag)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "blue")
    (Expr.apply_tag)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "green")
    (Expr.apply_tag)
  )
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
~~~

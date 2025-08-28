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
module [red, green, blue]

import Color
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
**Parse Error**
at 3:14 to 3:14

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_plus)
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

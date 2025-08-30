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

import Color*

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
at 3:14 to 5:1

**Unsupported Node**
at 3:1 to 3:14

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "red")
    (Expr.apply_tag)
  )
  (Expr.binop_equals
    (Expr.lookup "red")
    (Expr.apply_tag)
  )
  (Expr.binop_colon
    (Expr.lookup "blue")
    (Expr.apply_tag)
  )
  (Expr.binop_equals
    (Expr.lookup "blue")
    (Expr.apply_tag)
  )
  (Expr.binop_colon
    (Expr.lookup "green")
    (Expr.apply_tag)
  )
  (Expr.binop_equals
    (Expr.lookup "green")
    (Expr.apply_tag)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
red : []_others
blue : []_others
green : []_others
~~~

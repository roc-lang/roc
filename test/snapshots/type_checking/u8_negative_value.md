# META
~~~ini
description=U8 type annotation with negative value
type=file
~~~
# SOURCE
~~~roc
module []

x : U8
x = -1
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare LowerIdent OpColon UpperIdent LowerIdent OpAssign OpUnaryMinus Int ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "x")
    (uc "U8")
  )
  (binop_equals
    (lc "x")
    (unary_neg <unary>)
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
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

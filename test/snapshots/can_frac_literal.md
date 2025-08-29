# META
~~~ini
description=Float literal type inference
type=file
~~~
# SOURCE
~~~roc
module []

x = 3.14
y = 1.23e45
z = 0.5
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare LowerIdent OpAssign Float LowerIdent OpAssign Float LowerIdent OpAssign Float ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "x")
    (frac_literal_small 3.14)
  )
  (binop_equals
    (lc "y")
    (frac_literal_big big:<idx:0>)
  )
  (binop_equals
    (lc "z")
    (frac_literal_small 0.5)
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
  (Expr.binop_equals
    (Expr.lookup "x")
    (Expr.frac_literal_small 3.14)
  )
  (Expr.binop_equals
    (Expr.lookup "y")
    (Expr.frac_literal_big big:<idx:0>)
  )
  (Expr.binop_equals
    (Expr.lookup "z")
    (Expr.frac_literal_small 0.5)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
x : F64
y : F64
z : F64
~~~

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
module []


x = 3.14
y = 1.23e45
z = 0.5
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

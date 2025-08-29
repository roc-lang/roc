# META
~~~ini
description=Hexadecimal integer literal type inference
type=file
~~~
# SOURCE
~~~roc
module []

x = 0xFF
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare LowerIdent OpAssign Int ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "x")
    (num_literal_big big:<idx:0>)
  )
)
~~~
# FORMATTED
~~~roc
module []

x = 0xFF
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
    (Expr.num_literal_big)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
x : Num(_size)
~~~

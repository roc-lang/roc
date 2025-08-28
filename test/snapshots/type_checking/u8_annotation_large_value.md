# META
~~~ini
description=U8 type annotation with value exceeding U8 range
type=file
~~~
# SOURCE
~~~roc
module []

x : U8
x = 500
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare LowerIdent OpColon UpperIdent LowerIdent OpAssign Int ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "x")
    (uc "U8")
  )
  (binop_equals
    (lc "x")
    (num_literal_i32 500)
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
  (Expr.binop_colon
    (Expr.lookup "x")
    (Expr.apply_tag)
  )
  (Expr.binop_equals
    (Expr.lookup "x")
    (Expr.num_literal_i32 500)
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

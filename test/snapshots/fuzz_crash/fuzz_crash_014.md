# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
0b.0
0bu22
0u22
~~~
# TOKENS
~~~text
Int LowerIdent Dot Int Int LowerIdent Int LowerIdent ~~~
# PARSE
~~~clojure
(block
  (num_literal_i32 0)
  (binop_pipe
    (lc "b")
    (num_literal_i32 0)
  )
  (num_literal_i32 0)
  (lc "bu22")
  (num_literal_i32 0)
  (lc "u22")
)
~~~
# FORMATTED
~~~roc
0b.0b | 0
0bbu22
0u22
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_star)
  (Expr.frac_literal_big)
  (Expr.binop_star)
  (Expr.str_literal_big)
  (Expr.binop_star)
  (Expr.str_literal_big)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~

# META
~~~ini
description=Function call expression
type=expr
~~~
# SOURCE
~~~roc
add(5, 3)
~~~
# TOKENS
~~~text
LowerIdent OpenRound Int Comma Int CloseRound ~~~
# PARSE
~~~clojure
(apply_lc
  (lc "add")
  (tuple_literal
    (num_literal_i32 5)
    (num_literal_i32 3)
  )
)
~~~
# FORMATTED
~~~roc
add((5, 3))
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.binop_thick_arrow)
~~~
# SOLVED
~~~clojure
(expr :tag binop_thick_arrow :type "_a")
~~~
# TYPES
~~~roc
_a
~~~

# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
ff8.8.d
~~~
# TOKENS
~~~text
LowerIdent Dot Int Dot LowerIdent ~~~
# PARSE
~~~clojure
(block
  (binop_pipe
    (binop_pipe
      (lc "ff8")
      (num_literal_i32 8)
    )
    (dot_lc "d")
  )
)
~~~
# FORMATTED
~~~roc
(ff8 | 8.d) | .d
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.lambda)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~

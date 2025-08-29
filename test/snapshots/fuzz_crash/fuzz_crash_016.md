# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
0|
~~~
# TOKENS
~~~text
Int OpBar ~~~
# PARSE
~~~clojure
(block
  (binop_pipe
    (num_literal_i32 0)
    (malformed malformed:expr_unexpected_token)
  )
)
~~~
# FORMATTED
~~~roc
0 | 
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:3 to 1:3

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

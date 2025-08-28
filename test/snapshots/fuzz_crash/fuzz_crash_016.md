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
(Expr.record_access)
~~~
# SOLVED
~~~clojure
(expr :tag record_access :type "_a")
~~~
# TYPES
~~~roc
# File does not contain a block of statements
~~~

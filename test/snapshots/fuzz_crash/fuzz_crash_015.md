# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
0o0.0
0_0
0u8.0
0_
~~~
# TOKENS
~~~text
Int Dot Int Int Underscore Int Int LowerIdent Dot Int Int Underscore ~~~
# PARSE
~~~clojure
(block
  (binop_pipe
    (num_literal_big big:<idx:0>)
    (num_literal_i32 0)
  )
  (num_literal_i32 0)
  (underscore)
  (num_literal_i32 0)
  (num_literal_i32 0)
  (binop_pipe
    (lc "u8")
    (num_literal_i32 0)
  )
  (num_literal_i32 0)
  (underscore)
)
~~~
# FORMATTED
~~~roc
0o0.0 | 0
0_0_0
0u8 | 0
0__
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
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

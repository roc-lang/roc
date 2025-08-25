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
Int LowerIdent Dot Int Int Underscore Int Int LowerIdent Dot Int Int Underscore ~~~
# PARSE
~~~clojure
(block
  (num_literal_i32 0)
  (binop_pipe
    (lc "o0")
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
0
o0 | 0
0_0_0
0
u8 | 0
0__
~~~
# EXPECTED
NIL
# PROBLEMS
**Pattern in Expression Context**
at 2:2 to 2:3

**Pattern in Expression Context**
at 4:2 to 4:3

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.num_literal_i32 0)
  (Expr.lambda)
  (Expr.num_literal_i32 0)
  (Expr.malformed)
  (Expr.num_literal_i32 0)
  (Expr.num_literal_i32 0)
  (Expr.lambda)
  (Expr.num_literal_i32 0)
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

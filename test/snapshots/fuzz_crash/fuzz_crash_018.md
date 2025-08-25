# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
0 b:S
.R
~~~
# TOKENS
~~~text
Int LowerIdent OpColon UpperIdent Dot UpperIdent ~~~
# PARSE
~~~clojure
(block
  (num_literal_i32 0)
  (binop_colon
    (lc "b")
    (binop_pipe
      (uc "S")
      (uc "R")
    )
  )
)
~~~
# FORMATTED
~~~roc
0
b: S.R
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 1:5 to 1:6

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.num_literal_i32 0)
  (Expr.binop_colon
    (Expr.lookup "b")
    (Expr.lambda)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~

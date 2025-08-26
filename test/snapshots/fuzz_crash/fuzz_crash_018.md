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
b : S.R
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_star)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~

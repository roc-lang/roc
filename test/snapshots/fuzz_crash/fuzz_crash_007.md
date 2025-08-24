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
NO CHANGE
~~~
# EXPECTED
MISSING HEADER - fuzz_crash_007.md:1:1:1:4
PARSE ERROR - fuzz_crash_007.md:1:4:1:6
PARSE ERROR - fuzz_crash_007.md:1:6:1:8
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

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

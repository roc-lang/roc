# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
mo|%
~~~
# TOKENS
~~~text
LowerIdent OpBar MalformedUnknownToken ~~~
# PARSE
~~~clojure
(block
  (binop_pipe
    (lc "mo")
    (malformed malformed:expr_unexpected_token)
  )
)
~~~
# FORMATTED
~~~roc
mo | %
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:4 to 1:4

**Unsupported Node**
at 1:4 to 1:4

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.lambda)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_arg -> Error")
~~~
# TYPES
~~~roc
~~~

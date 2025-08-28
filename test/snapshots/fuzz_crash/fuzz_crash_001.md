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
mo | 
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:4 to 1:4

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

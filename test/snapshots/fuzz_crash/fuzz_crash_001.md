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
  (lc "mo")
  (malformed malformed:expr_unexpected_token)
)
~~~
# FORMATTED
~~~roc
mo
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:4 to 1:5

**Parse Error**
at 1:5 to 1:5

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.lookup "mo")
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

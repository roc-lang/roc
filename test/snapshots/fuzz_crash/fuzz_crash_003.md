# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
= "te
~~~
# TOKENS
~~~text
OpAssign MalformedString ~~~
# PARSE
~~~clojure
(block
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
)
~~~
# FORMATTED
~~~roc
<malformed><malformed>
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 1:1

**Parse Error**
at 1:3 to 1:3

**Unsupported Node**
at 1:2 to 1:2

**Unsupported Node**
at 1:3 to 1:3

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
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

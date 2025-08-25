# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]"\
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare MalformedString ~~~
# PARSE
~~~clojure
(block
  (malformed malformed:expr_unexpected_token)
)
~~~
# FORMATTED
~~~roc
module []

<malformed>
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:9 to 1:9

**Unsupported Node**
at 1:9 to 1:9

# CANONICALIZE
~~~clojure
(Expr.block
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

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

~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:9 to 1:9

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

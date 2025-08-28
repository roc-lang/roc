# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]!0.t
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare OpBang Int Dot LowerIdent ~~~
# PARSE
~~~clojure
(block
  (unary_not <unary>)
)
~~~
# FORMATTED
~~~roc
module []

!0 | .t
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

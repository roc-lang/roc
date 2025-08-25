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

0 | .t!
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.unary_not)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "[True, False]_others")
~~~
# TYPES
~~~roc
~~~

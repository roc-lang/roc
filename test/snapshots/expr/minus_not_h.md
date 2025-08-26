# META
~~~ini
description=Unary minus and boolean not (should error)
type=expr
~~~
# SOURCE
~~~roc
-!h
~~~
# TOKENS
~~~text
OpUnaryMinus OpBang LowerIdent ~~~
# PARSE
~~~clojure
(unary_neg <unary>)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.unary_double_dot)
~~~
# SOLVED
~~~clojure
(expr :tag unary_double_dot :type "_a")
~~~
# TYPES
~~~roc
_a
~~~

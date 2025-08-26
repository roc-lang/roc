# META
~~~ini
description=Negative float literal
type=expr
~~~
# SOURCE
~~~roc
-2.5
~~~
# TOKENS
~~~text
OpUnaryMinus Float ~~~
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

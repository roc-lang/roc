# META
~~~ini
description=Unary not operation expression
type=expr
~~~
# SOURCE
~~~roc
!isValid
~~~
# TOKENS
~~~text
OpBang LowerIdent ~~~
# PARSE
~~~clojure
(unary_not <unary>)
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

# META
~~~ini
description=Negative single quote char literal
type=expr
~~~
# SOURCE
~~~roc
-'i'
~~~
# TOKENS
~~~text
OpUnaryMinus SingleQuote ~~~
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

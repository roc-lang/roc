# META
~~~ini
description=min_parens_number
type=expr
~~~
# SOURCE
~~~roc
-(8)
~~~
# TOKENS
~~~text
OpUnaryMinus OpenRound Int CloseRound ~~~
# PARSE
~~~clojure
(unary_neg <unary>)
~~~
# FORMATTED
~~~roc
-8
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

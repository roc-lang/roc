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
(unary_neg <unary_op>)
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
(Expr.unary_neg)
~~~
# SOLVED
~~~clojure
(expr :tag unary_neg :type "Num(_a)")
~~~
# TYPES
~~~roc
Num(_a)
~~~

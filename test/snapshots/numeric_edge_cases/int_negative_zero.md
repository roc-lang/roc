# META
~~~ini
description=Negative integer zero
type=expr
~~~
# SOURCE
~~~roc
-0
~~~
# TOKENS
~~~text
OpUnaryMinus Int ~~~
# PARSE
~~~clojure
(unary_neg <unary_op>)
~~~
# FORMATTED
~~~roc
-0
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

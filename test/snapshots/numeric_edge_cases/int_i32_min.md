# META
~~~ini
description=Minimum value for i32 (-2147483648)
type=expr
~~~
# SOURCE
~~~roc
-2147483648
~~~
# TOKENS
~~~text
OpUnaryMinus Int ~~~
# PARSE
~~~clojure
(unary_neg <unary>)
~~~
# FORMATTED
~~~roc
-2147483648
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

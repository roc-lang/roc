# META
~~~ini
description=Negative integer literal canonicalization
type=expr
~~~
# SOURCE
~~~roc
-123
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
-123
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

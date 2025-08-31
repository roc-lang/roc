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
(unary_neg <unary_op>)
~~~
# FORMATTED
~~~roc
-'i'
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

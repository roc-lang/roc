# META
~~~ini
description=Minimum negative value that fits in dec_small (i16 min)
type=expr
~~~
# SOURCE
~~~roc
-327.68
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
-327.68
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
(expr :tag unary_neg :type "F64")
~~~
# TYPES
~~~roc
F64
~~~

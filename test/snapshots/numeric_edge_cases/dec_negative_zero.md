# META
~~~ini
description=Negative zero as a decimal literal
type=expr
~~~
# SOURCE
~~~roc
-0.0
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
(Expr.malformed)
~~~
# SOLVED
~~~clojure
(expr :tag malformed :type "Error")
~~~
# TYPES
~~~roc
Error
~~~

# META
~~~ini
description=Negative float literal
type=expr
~~~
# SOURCE
~~~roc
-2.5
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

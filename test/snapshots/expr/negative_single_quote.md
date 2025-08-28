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

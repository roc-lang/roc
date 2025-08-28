# META
~~~ini
description=Unary minus and boolean not (should error)
type=expr
~~~
# SOURCE
~~~roc
-!h
~~~
# TOKENS
~~~text
OpUnaryMinus OpBang LowerIdent ~~~
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

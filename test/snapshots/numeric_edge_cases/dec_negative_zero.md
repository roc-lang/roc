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
(unary_neg <unary_op>)
~~~
# FORMATTED
~~~roc
-0.0
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
; Total type variables: 3
(var #0 _)
(var #1 F64)
(var #2 -> #1)
~~~
# TYPES
~~~roc
~~~

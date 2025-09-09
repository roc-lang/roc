# META
~~~ini
description=Small negative decimal that fits in dec_small
type=expr
~~~
# SOURCE
~~~roc
-3.14159
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
-3.14159
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

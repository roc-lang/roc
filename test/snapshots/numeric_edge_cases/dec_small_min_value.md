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
(unary_neg <unary_op>)
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
; Total type variables: 3
(var #0 _)
(var #1 F64)
(var #2 -> #1)
~~~
# TYPES
~~~roc
~~~

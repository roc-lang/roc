# META
~~~ini
description=Minimum value for i32 (-2147483648)
type=expr
~~~
# SOURCE
~~~roc
-2147483648
~~~
# TOKENS
~~~text
OpUnaryMinus Int ~~~
# PARSE
~~~clojure
(unary_neg <unary_op>)
~~~
# FORMATTED
~~~roc
-2147483648
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
(var #1 Num *)
(var #2 -> #1)
~~~
# TYPES
~~~roc
~~~

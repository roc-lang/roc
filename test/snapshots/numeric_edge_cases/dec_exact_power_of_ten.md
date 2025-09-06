# META
~~~ini
description=Decimal that is exactly 1/10^n
type=expr
~~~
# SOURCE
~~~roc
0.001
~~~
# TOKENS
~~~text
Float ~~~
# PARSE
~~~clojure
(frac_literal_small 0.001)
~~~
# FORMATTED
~~~roc
0.001
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.frac_literal_small 0.001)
~~~
# SOLVED
~~~clojure
; Total type variables: 2
(var #0 _)
(var #1 F64)
~~~
# TYPES
~~~roc
~~~

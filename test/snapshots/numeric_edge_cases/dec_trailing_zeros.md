# META
~~~ini
description=Decimal with trailing zeros
type=expr
~~~
# SOURCE
~~~roc
1.2000
~~~
# TOKENS
~~~text
Float ~~~
# PARSE
~~~clojure
(frac_literal_small 1.2)
~~~
# FORMATTED
~~~roc
1.2000
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.frac_literal_small 1.2)
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

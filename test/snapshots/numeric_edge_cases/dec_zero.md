# META
~~~ini
description=Zero as a decimal literal
type=expr
~~~
# SOURCE
~~~roc
0.0
~~~
# TOKENS
~~~text
Float ~~~
# PARSE
~~~clojure
(frac_literal_small 0)
~~~
# FORMATTED
~~~roc
0.0
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.frac_literal_small 0)
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

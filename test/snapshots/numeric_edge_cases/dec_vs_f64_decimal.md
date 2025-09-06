# META
~~~ini
description=Same number as dec_vs_f64_scientific but in decimal form
type=expr
~~~
# SOURCE
~~~roc
150.0
~~~
# TOKENS
~~~text
Float ~~~
# PARSE
~~~clojure
(frac_literal_small 150)
~~~
# FORMATTED
~~~roc
150.0
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.frac_literal_small 150)
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

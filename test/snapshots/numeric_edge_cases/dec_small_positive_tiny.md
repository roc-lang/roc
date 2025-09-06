# META
~~~ini
description=Tiny positive decimal that fits in dec_small
type=expr
~~~
# SOURCE
~~~roc
0.0001
~~~
# TOKENS
~~~text
Float ~~~
# PARSE
~~~clojure
(frac_literal_small 0.0001)
~~~
# FORMATTED
~~~roc
0.0001
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.frac_literal_small 0.0001)
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

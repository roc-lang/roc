# META
~~~ini
description=Maximum positive value that fits in dec_small (i16 max)
type=expr
~~~
# SOURCE
~~~roc
327.67
~~~
# TOKENS
~~~text
Float ~~~
# PARSE
~~~clojure
(frac_literal_small 327.67)
~~~
# FORMATTED
~~~roc
327.67
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.frac_literal_small 327.67)
~~~
# SOLVED
~~~clojure
(expr :tag frac_literal_small :type "F64")
~~~
# TYPES
~~~roc
F64
~~~

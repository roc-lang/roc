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
NO CHANGE
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.binop_not_equals)
~~~
# SOLVED
~~~clojure
(expr :tag binop_not_equals :type "[True, False]_others")
~~~
# TYPES
~~~roc
[True, False]_others
~~~

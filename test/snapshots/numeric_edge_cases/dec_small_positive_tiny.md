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
NO CHANGE
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.binop_double_equals)
~~~
# SOLVED
~~~clojure
(expr :tag binop_double_equals :type "[True, False]_others")
~~~
# TYPES
~~~roc
[True, False]_others
~~~

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

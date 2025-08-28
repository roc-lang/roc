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

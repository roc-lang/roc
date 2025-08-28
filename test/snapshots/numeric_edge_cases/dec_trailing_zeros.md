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

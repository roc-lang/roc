# META
~~~ini
description=Maximum value for i128 (170141183460469231731687303715884105727)
type=expr
~~~
# SOURCE
~~~roc
170141183460469231731687303715884105727
~~~
# TOKENS
~~~text
Int ~~~
# PARSE
~~~clojure
(num_literal_big big:<idx:0>)
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
(Expr.binop_gte)
~~~
# SOLVED
~~~clojure
(expr :tag binop_gte :type "[True, False]_others")
~~~
# TYPES
~~~roc
[True, False]_others
~~~

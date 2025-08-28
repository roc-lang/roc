# META
~~~ini
description=Maximum value for i64 (9223372036854775807)
type=expr
~~~
# SOURCE
~~~roc
9223372036854775807
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

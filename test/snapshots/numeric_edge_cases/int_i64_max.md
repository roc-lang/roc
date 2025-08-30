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
(Expr.num_literal_big)
~~~
# SOLVED
~~~clojure
(expr :tag num_literal_big :type "Num(_a)")
~~~
# TYPES
~~~roc
Num(_a)
~~~

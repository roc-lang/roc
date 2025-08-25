# META
~~~ini
description=Maximum value for i32 (2147483647)
type=expr
~~~
# SOURCE
~~~roc
2147483647
~~~
# TOKENS
~~~text
Int ~~~
# PARSE
~~~clojure
(num_literal_i32 2147483647)
~~~
# FORMATTED
~~~roc
2147483647
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.num_literal_i32 2147483647)
~~~
# SOLVED
~~~clojure
(expr :tag num_literal_i32 :type "Num(_a)")
~~~
# TYPES
~~~roc
Num(_a)
~~~

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
NO CHANGE
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.binop_slash)
~~~
# SOLVED
~~~clojure
(expr :tag binop_slash :type "Num(_a)")
~~~
# TYPES
~~~roc
Num(_a)
~~~

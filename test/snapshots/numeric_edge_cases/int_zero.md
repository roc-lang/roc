# META
~~~ini
description=Integer zero
type=expr
~~~
# SOURCE
~~~roc
0
~~~
# TOKENS
~~~text
Int ~~~
# PARSE
~~~clojure
(num_literal_i32 0)
~~~
# FORMATTED
~~~roc
0
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.num_literal_i32 0)
~~~
# SOLVED
~~~clojure
(expr :tag num_literal_i32 :type "Num(_a)")
~~~
# TYPES
~~~roc
Num(_a)
~~~

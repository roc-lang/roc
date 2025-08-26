# META
~~~ini
description=Maximum value for i8 (127)
type=expr
~~~
# SOURCE
~~~roc
127
~~~
# TOKENS
~~~text
Int ~~~
# PARSE
~~~clojure
(num_literal_i32 127)
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
(Expr.binop_star)
~~~
# SOLVED
~~~clojure
(expr :tag binop_star :type "Num(_a)")
~~~
# TYPES
~~~roc
Num(_a)
~~~

# META
~~~ini
description=Maximum value for u8 (255)
type=expr
~~~
# SOURCE
~~~roc
255
~~~
# TOKENS
~~~text
Int ~~~
# PARSE
~~~clojure
(num_literal_i32 255)
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

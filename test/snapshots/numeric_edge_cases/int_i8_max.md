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

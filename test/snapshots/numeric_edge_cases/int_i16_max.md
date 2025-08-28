# META
~~~ini
description=Maximum value for i16 (32767)
type=expr
~~~
# SOURCE
~~~roc
32767
~~~
# TOKENS
~~~text
Int ~~~
# PARSE
~~~clojure
(num_literal_i32 32767)
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

# META
~~~ini
description=Heterogeneous list causes type mismatch
type=expr
~~~
# SOURCE
~~~roc
[1, "hello", 3.14]
~~~
# TOKENS
~~~text
OpenSquare Int Comma String Comma Float CloseSquare ~~~
# PARSE
~~~clojure
(list_literal
  (num_literal_i32 1)
  (str_literal_big "hello")
  (frac_literal_small 3.14)
)
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
(Expr.binop_or)
~~~
# SOLVED
~~~clojure
(expr :tag binop_or :type "[True, False]_others")
~~~
# TYPES
~~~roc
[True, False]_others
~~~

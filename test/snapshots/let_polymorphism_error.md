# META
~~~ini
description=Let-polymorphism error case - incompatible list elements
type=expr
~~~
# SOURCE
~~~roc
[42, 4.2, "hello"]
~~~
# TOKENS
~~~text
OpenSquare Int Comma Float Comma String CloseSquare ~~~
# PARSE
~~~clojure
(list_literal
  (num_literal_i32 42)
  (frac_literal_small 4.2)
  (str_literal_big "hello")
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

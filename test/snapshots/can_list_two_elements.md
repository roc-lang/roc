# META
~~~ini
description=List with exactly two incompatible elements
type=expr
~~~
# SOURCE
~~~roc
[1, "hello"]
~~~
# TOKENS
~~~text
OpenSquare Int Comma String CloseSquare ~~~
# PARSE
~~~clojure
(list_literal
  (num_literal_i32 1)
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

# META
~~~ini
description=List with integer literals
type=expr
~~~
# SOURCE
~~~roc
[1, 2, "hello"]
~~~
# TOKENS
~~~text
OpenSquare Int Comma Int Comma String CloseSquare ~~~
# PARSE
~~~clojure
(list_literal
  (num_literal_i32 1)
  (num_literal_i32 2)
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
(Expr.binop_double_question)
~~~
# SOLVED
~~~clojure
(expr :tag binop_double_question :type "_a")
~~~
# TYPES
~~~roc
_a
~~~

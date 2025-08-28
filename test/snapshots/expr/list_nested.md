# META
~~~ini
description=Nested list literals
type=expr
~~~
# SOURCE
~~~roc
[[1, 2], [3, 4], [5]]
~~~
# TOKENS
~~~text
OpenSquare OpenSquare Int Comma Int CloseSquare Comma OpenSquare Int Comma Int CloseSquare Comma OpenSquare Int CloseSquare CloseSquare ~~~
# PARSE
~~~clojure
(list_literal
  (list_literal
    (num_literal_i32 1)
    (num_literal_i32 2)
  )
  (list_literal
    (num_literal_i32 3)
    (num_literal_i32 4)
  )
  (list_literal
    (num_literal_i32 5)
  )
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

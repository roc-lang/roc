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
[[1, 2], [3, 4], [5]]
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.list_literal)
~~~
# SOLVED
~~~clojure
; Total type variables: 14
(var #0 _)
(var #1 Num *)
(var #2 -> #1)
(var #3 -> #10)
(var #4 -> #1)
(var #5 -> #4)
(var #6 -> #11)
(var #7 -> #1)
(var #8 -> #12)
(var #9 -> #13)
(var #10 List #1)
(var #11 -> #10)
(var #12 -> #10)
(var #13 List #3)
~~~
# TYPES
~~~roc
~~~

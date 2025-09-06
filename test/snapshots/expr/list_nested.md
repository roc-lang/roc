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
; Total type variables: 10
(var #0 _)
(var #1 Num *)
(var #2 Num *)
(var #3 _)
(var #4 Num *)
(var #5 Num *)
(var #6 _)
(var #7 Num *)
(var #8 _)
(var #9 _)
~~~
# TYPES
~~~roc
~~~

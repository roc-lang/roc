# META
~~~ini
description=Nested heterogeneous lists
type=expr
~~~
# SOURCE
~~~roc
[[1, "hello"], [2, 3]]
~~~
# TOKENS
~~~text
OpenSquare OpenSquare Int Comma String CloseSquare Comma OpenSquare Int Comma Int CloseSquare CloseSquare ~~~
# PARSE
~~~clojure
(list_literal
  (list_literal
    (num_literal_i32 1)
    (str_literal_big "hello")
  )
  (list_literal
    (num_literal_i32 2)
    (num_literal_i32 3)
  )
)
~~~
# FORMATTED
~~~roc
[[1, "hello"], [2, 3]]
~~~
# EXPECTED
INCOMPATIBLE LIST ELEMENTS - can_nested_heterogeneous_lists.md:1:3:1:3
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.list_literal)
~~~
# SOLVED
~~~clojure
; Total type variables: 11
(var #0 _)
(var #1 <error>)
(var #2 -> #1)
(var #3 -> #8)
(var #4 -> #1)
(var #5 -> #4)
(var #6 -> #9)
(var #7 -> #10)
(var #8 List #1)
(var #9 -> #8)
(var #10 List #3)
~~~
# TYPES
~~~roc
~~~

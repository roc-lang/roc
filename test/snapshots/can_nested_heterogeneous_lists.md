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
~~~
# TYPES
~~~roc
# No header found
~~~

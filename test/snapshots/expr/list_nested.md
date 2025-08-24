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
  (tuple_literal
    (list_literal
      (tuple_literal
        (num_literal_i32 1)
        (num_literal_i32 2)
      )
    )
    (list_literal
      (tuple_literal
        (num_literal_i32 3)
        (num_literal_i32 4)
      )
    )
    (list_literal
      (num_literal_i32 5)
    )
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
**Unsupported Node**
at 1:1 to 1:21

# CANONICALIZE
~~~clojure
(Stmt.malformed)
~~~
# SOLVED
~~~clojure
; No expression to type check
~~~
# TYPES
~~~roc
# No expression found
~~~

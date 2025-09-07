# META
~~~ini
description=List with type mismatch followed by nested heterogeneous list
type=expr
~~~
# SOURCE
~~~roc
[1, "hello", [3, "world"]]
~~~
# TOKENS
~~~text
OpenSquare Int Comma String Comma OpenSquare Int Comma String CloseSquare CloseSquare ~~~
# PARSE
~~~clojure
(list_literal
  (num_literal_i32 1)
  (str_literal_big "hello")
  (list_literal
    (num_literal_i32 3)
    (str_literal_big "world")
  )
)
~~~
# FORMATTED
~~~roc
[1, "hello", [3, "world"]]
~~~
# EXPECTED
INCOMPATIBLE LIST ELEMENTS - can_list_mismatch_then_nested_error.md:1:2:1:2
INCOMPATIBLE LIST ELEMENTS - can_list_mismatch_then_nested_error.md:1:15:1:15
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.list_literal)
~~~
# SOLVED
~~~clojure
; Total type variables: 9
(var #0 _)
(var #1 <error>)
(var #2 -> #1)
(var #3 <error>)
(var #4 -> #3)
(var #5 -> #7)
(var #6 -> #8)
(var #7 -> #1)
(var #8 List #1)
~~~
# TYPES
~~~roc
~~~

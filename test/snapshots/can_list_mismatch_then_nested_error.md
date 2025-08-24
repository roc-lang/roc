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
  (tuple_literal
    (num_literal_i32 1)
    (str_literal_big "hello")
    (list_literal
      (tuple_literal
        (num_literal_i32 3)
        (str_literal_big "world")
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
INCOMPATIBLE LIST ELEMENTS - can_list_mismatch_then_nested_error.md:1:2:1:2
INCOMPATIBLE LIST ELEMENTS - can_list_mismatch_then_nested_error.md:1:15:1:15
# PROBLEMS
**Unsupported Node**
at 1:1 to 1:26

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

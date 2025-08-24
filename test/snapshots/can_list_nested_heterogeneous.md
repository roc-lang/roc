# META
~~~ini
description=Heterogeneous nested list causes type mismatch
type=expr
~~~
# SOURCE
~~~roc
[[], [1], ["hello"]]
~~~
# TOKENS
~~~text
OpenSquare OpenSquare CloseSquare Comma OpenSquare Int CloseSquare Comma OpenSquare String CloseSquare CloseSquare ~~~
# PARSE
~~~clojure
(list_literal
  (tuple_literal
    (list_literal)
    (list_literal
      (num_literal_i32 1)
    )
    (list_literal
      (str_literal_big "hello")
    )
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
INCOMPATIBLE LIST ELEMENTS - can_list_nested_heterogeneous.md:1:6:1:6
# PROBLEMS
**Unsupported Node**
at 1:1 to 1:20

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

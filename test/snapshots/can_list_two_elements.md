# META
~~~ini
description=List with exactly two incompatible elements
type=expr
~~~
# SOURCE
~~~roc
[1, "hello"]
~~~
# TOKENS
~~~text
OpenSquare Int Comma String CloseSquare ~~~
# PARSE
~~~clojure
(list_literal
  (num_literal_i32 1)
  (str_literal_big "hello")
)
~~~
# FORMATTED
~~~roc
[1, "hello"]
~~~
# EXPECTED
INCOMPATIBLE LIST ELEMENTS - can_list_two_elements.md:1:2:1:2
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.list_literal)
~~~
# SOLVED
~~~clojure
; Total type variables: 5
(var #0 _)
(var #1 <error>)
(var #2 -> #1)
(var #3 -> #4)
(var #4 List #1)
~~~
# TYPES
~~~roc
~~~

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
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.list_literal)
~~~
# SOLVED
~~~clojure
(expr :tag list_literal :type "List(Error)")
~~~
# TYPES
~~~roc
List(Error)
~~~

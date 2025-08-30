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
  (list_literal)
  (list_literal
    (num_literal_i32 1)
  )
  (list_literal
    (str_literal_big "hello")
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
NIL
# CANONICALIZE
~~~clojure
(Expr.list_literal)
~~~
# SOLVED
~~~clojure
(expr :tag list_literal :type "List(_a)")
~~~
# TYPES
~~~roc
List(_a)
~~~

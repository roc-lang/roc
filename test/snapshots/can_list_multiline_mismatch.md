# META
~~~ini
description=Multiline list with type mismatch
type=expr
~~~
# SOURCE
~~~roc
[
    42,
    "hello world",
    100
]
~~~
# TOKENS
~~~text
OpenSquare Int Comma String Comma Int CloseSquare ~~~
# PARSE
~~~clojure
(list_literal
  (num_literal_i32 42)
  (str_literal_big "hello world")
  (num_literal_i32 100)
)
~~~
# FORMATTED
~~~roc
[42, "hello world", 100]
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

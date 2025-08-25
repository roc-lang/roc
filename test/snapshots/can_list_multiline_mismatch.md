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
  (tuple_literal
    (num_literal_i32 42)
    (str_literal_big "hello world")
    (num_literal_i32 100)
  )
)
~~~
# FORMATTED
~~~roc
[(42, "hello world", 100)]
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 1:1 to 5:1

# CANONICALIZE
~~~clojure
(Expr.malformed)
~~~
# SOLVED
~~~clojure
(expr :tag malformed :type "Error")
~~~
# TYPES
~~~roc
Error
~~~

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
INCOMPATIBLE LIST ELEMENTS - can_list_multiline_mismatch.md:2:5:2:5
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
(var #1 Num *)
(var #2 Str)
(var #3 Num *)
(var #4 _)
~~~
# TYPES
~~~roc
~~~

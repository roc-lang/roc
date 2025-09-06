# META
~~~ini
description=List with integer literals
type=expr
~~~
# SOURCE
~~~roc
[1, 2, "hello"]
~~~
# TOKENS
~~~text
OpenSquare Int Comma Int Comma String CloseSquare ~~~
# PARSE
~~~clojure
(list_literal
  (num_literal_i32 1)
  (num_literal_i32 2)
  (str_literal_big "hello")
)
~~~
# FORMATTED
~~~roc
[1, 2, "hello"]
~~~
# EXPECTED
INCOMPATIBLE LIST ELEMENTS - list_type_err.md:1:5:1:5
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
(var #2 Num *)
(var #3 Str)
(var #4 _)
~~~
# TYPES
~~~roc
~~~

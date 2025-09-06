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
[[], [1], ["hello"]]
~~~
# EXPECTED
INCOMPATIBLE LIST ELEMENTS - can_list_nested_heterogeneous.md:1:6:1:6
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.list_literal)
~~~
# SOLVED
~~~clojure
; Total type variables: 7
(var #0 _)
(var #1 _)
(var #2 Num *)
(var #3 _)
(var #4 Str)
(var #5 _)
(var #6 _)
~~~
# TYPES
~~~roc
~~~

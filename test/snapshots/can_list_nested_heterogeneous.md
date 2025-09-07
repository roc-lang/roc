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
; Total type variables: 12
(var #0 _)
(var #1 -> #8)
(var #2 -> #7)
(var #3 -> #9)
(var #4 Str)
(var #5 -> #1)
(var #6 -> #11)
(var #7 Num *)
(var #8 <error>)
(var #9 -> #8)
(var #10 List #4)
(var #11 List #1)
~~~
# TYPES
~~~roc
~~~

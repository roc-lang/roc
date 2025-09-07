# META
~~~ini
description=Triply-nested heterogeneous list causes type mismatch
type=expr
~~~
# SOURCE
~~~roc
[[], [[], [1]], [[], ["hello"]]]
~~~
# TOKENS
~~~text
OpenSquare OpenSquare CloseSquare Comma OpenSquare OpenSquare CloseSquare Comma OpenSquare Int CloseSquare CloseSquare Comma OpenSquare OpenSquare CloseSquare Comma OpenSquare String CloseSquare CloseSquare CloseSquare ~~~
# PARSE
~~~clojure
(list_literal
  (list_literal)
  (list_literal
    (list_literal)
    (list_literal
      (num_literal_i32 1)
    )
  )
  (list_literal
    (list_literal)
    (list_literal
      (str_literal_big "hello")
    )
  )
)
~~~
# FORMATTED
~~~roc
[[], [[], [1]], [[], ["hello"]]]
~~~
# EXPECTED
INCOMPATIBLE LIST ELEMENTS - can_list_triple_nested_heterogeneous.md:1:6:1:6
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.list_literal)
~~~
# SOLVED
~~~clojure
; Total type variables: 22
(var #0 _)
(var #1 -> #12)
(var #2 -> #14)
(var #3 -> #13)
(var #4 -> #15)
(var #5 -> #16)
(var #6 -> #18)
(var #7 -> #17)
(var #8 -> #19)
(var #9 -> #1)
(var #10 -> #21)
(var #11 List #13)
(var #12 <error>)
(var #13 Num *)
(var #14 -> #11)
(var #15 -> #14)
(var #16 -> #12)
(var #17 Str)
(var #18 List #17)
(var #19 -> #18)
(var #20 List #6)
(var #21 List #1)
~~~
# TYPES
~~~roc
~~~

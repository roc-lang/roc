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
~~~
# TYPES
~~~roc
# No header found
~~~

# META
~~~ini
description=Tuple unification with different types
type=expr
~~~
# SOURCE
~~~roc
[(1, "a"), (2.5, "b")]
~~~
# TOKENS
~~~text
OpenSquare OpenRound Int Comma String CloseRound Comma OpenRound Float Comma String CloseRound CloseSquare ~~~
# PARSE
~~~clojure
(list_literal
  (tuple_literal
    (tuple_literal
      (num_literal_i32 1)
      (str_literal_small "a")
    )
    (tuple_literal
      (frac_literal_small 2.5)
      (str_literal_small "b")
    )
  )
)
~~~
# FORMATTED
~~~roc
[((1, "a"), (2.5, "b"))]
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 1:1 to 1:22

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

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
    (num_literal_i32 1)
    (str_literal_small "a")
  )
  (tuple_literal
    (frac_literal_small 2.5)
    (str_literal_small "b")
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

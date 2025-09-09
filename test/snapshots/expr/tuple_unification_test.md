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
[(1, "a"), (2.5, "b")]
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
; Total type variables: 11
(var #0 _)
(var #1 Num *)
(var #2 Str)
(var #3 -> #8)
(var #4 F64)
(var #5 Str)
(var #6 -> #3)
(var #7 -> #10)
(var #8 <error>)
(var #9 tuple)
(var #10 List #3)
~~~
# TYPES
~~~roc
~~~

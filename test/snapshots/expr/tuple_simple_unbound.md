# META
~~~ini
description=Simple tuple literal
type=expr
~~~
# SOURCE
~~~roc
(1, "hello", True)
~~~
# TOKENS
~~~text
OpenRound Int Comma String Comma UpperIdent CloseRound ~~~
# PARSE
~~~clojure
(tuple_literal
  (num_literal_i32 1)
  (str_literal_big "hello")
  (uc "True")
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
(Expr.tuple_literal
  (Expr.num_literal_i32 1)
  (Expr.str_literal_big)
  (Expr.apply_tag)
)
~~~
# SOLVED
~~~clojure
(expr :tag tuple_literal :type "_a")
~~~
# TYPES
~~~roc
_a
~~~

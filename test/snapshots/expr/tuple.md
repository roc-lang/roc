# META
~~~ini
description=Tuple expression
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
(1, "hello", True)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.tuple_literal)
~~~
# SOLVED
~~~clojure
(expr :tag tuple_literal :type "_a")
~~~
# TYPES
~~~roc
_a
~~~

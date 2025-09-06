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
(Expr.tuple_literal
  (Expr.num_literal_i32 1)
  (Expr.str_literal_big)
  (Expr.tag_no_args)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 6
(var #0 _)
(var #1 Num *)
(var #2 Str)
(var #3 _)
(var #4 -> #5)
(var #5 tuple)
~~~
# TYPES
~~~roc
~~~

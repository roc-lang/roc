# META
~~~ini
description=Function application expression
type=expr
~~~
# SOURCE
~~~roc
foo(42, "hello")
~~~
# TOKENS
~~~text
LowerIdent OpenRound Int Comma String CloseRound ~~~
# PARSE
~~~clojure
(apply_lc
  (lc "foo")
  (tuple_literal
    (num_literal_i32 42)
    (str_literal_big "hello")
  )
)
~~~
# FORMATTED
~~~roc
foo((42, "hello"))
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.binop_thin_arrow)
~~~
# SOLVED
~~~clojure
(expr :tag binop_thin_arrow :type "_a")
~~~
# TYPES
~~~roc
_a
~~~

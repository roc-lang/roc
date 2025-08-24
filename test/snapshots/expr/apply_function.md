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
NO CHANGE
~~~
# EXPECTED
UNDEFINED VARIABLE - apply_function.md:1:1:1:4
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.apply_ident)
~~~
# SOLVED
~~~clojure
(expr :tag apply_ident :type "_a")
~~~
# TYPES
~~~roc
_a
~~~

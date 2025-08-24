# META
~~~ini
description=Function call expression
type=expr
~~~
# SOURCE
~~~roc
add(5, 3)
~~~
# TOKENS
~~~text
LowerIdent OpenRound Int Comma Int CloseRound ~~~
# PARSE
~~~clojure
(apply_lc
  (lc "add")
  (tuple_literal
    (num_literal_i32 5)
    (num_literal_i32 3)
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
UNDEFINED VARIABLE - function_call.md:1:1:1:4
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

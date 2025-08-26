# META
~~~ini
description=Record field access with function call
type=expr
~~~
# SOURCE
~~~roc
(person.transform)(42)
~~~
# TOKENS
~~~text
OpenRound LowerIdent Dot LowerIdent CloseRound OpenRound Int CloseRound ~~~
# PARSE
~~~clojure
(apply_anon
  (binop_pipe
    (lc "person")
    (dot_lc "transform")
  )
  (num_literal_i32 42)
)
~~~
# FORMATTED
~~~roc
person.transform(42)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.binop_colon)
~~~
# SOLVED
~~~clojure
(expr :tag binop_colon :type "_a")
~~~
# TYPES
~~~roc
_a
~~~

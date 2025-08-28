# META
~~~ini
description=Tag with payload
type=expr
~~~
# SOURCE
~~~roc
Some(42)
~~~
# TOKENS
~~~text
UpperIdent OpenRound Int CloseRound ~~~
# PARSE
~~~clojure
(apply_uc
  (uc "Some")
  (num_literal_i32 42)
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

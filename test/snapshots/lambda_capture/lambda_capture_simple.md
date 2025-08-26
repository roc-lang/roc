# META
~~~ini
description=Simple lambda execution test to verify basic functionality works
type=expr
~~~
# SOURCE
~~~roc
(|x| x + 1)(5)
~~~
# TOKENS
~~~text
OpenRound OpBar LowerIdent OpBar LowerIdent OpPlus Int CloseRound OpenRound Int CloseRound ~~~
# PARSE
~~~clojure
(apply_anon
  (lambda
    (body
      (binop_plus
        (lc "x")
        (num_literal_i32 1)
      )
    )
    (args
      (lc "x")
    )
  )
  (num_literal_i32 5)
)
~~~
# FORMATTED
~~~roc
\x -> x + 1(5)
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

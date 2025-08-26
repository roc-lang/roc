# META
~~~ini
description=Unary minus operation on lambda parameter
type=expr
~~~
# SOURCE
~~~roc
(|x| -x)(5)
~~~
# TOKENS
~~~text
OpenRound OpBar LowerIdent OpBar OpUnaryMinus LowerIdent CloseRound OpenRound Int CloseRound ~~~
# PARSE
~~~clojure
(apply_anon
  (lambda
    (body
      (unary_neg <unary>)
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
\x -> -x(5)
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

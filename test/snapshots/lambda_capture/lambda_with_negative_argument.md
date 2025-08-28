# META
~~~ini
description=Lambda function called with negative argument
type=expr
~~~
# SOURCE
~~~roc
(|x| x + 1)(-5)
~~~
# TOKENS
~~~text
OpenRound OpBar LowerIdent OpBar LowerIdent OpPlus Int CloseRound OpenRound OpUnaryMinus Int CloseRound ~~~
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
  (unary_neg <unary>)
)
~~~
# FORMATTED
~~~roc
|x| x + 1(-5)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.binop_equals)
~~~
# SOLVED
~~~clojure
(expr :tag binop_equals :type "_a")
~~~
# TYPES
~~~roc
_a
~~~

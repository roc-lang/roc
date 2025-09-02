# META
~~~ini
description=Double negative unary minus operation
type=expr
~~~
# SOURCE
~~~roc
(|x| -(-x))(5)
~~~
# TOKENS
~~~text
OpenRound OpBar LowerIdent OpBar OpUnaryMinus OpenRound OpUnaryMinus LowerIdent CloseRound CloseRound OpenRound Int CloseRound ~~~
# PARSE
~~~clojure
(apply_anon
  (lambda
    (body
      (unary_neg <unary_op>)
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
(|x| --x)(5)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.apply_ident)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
# No header found
~~~

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
(|x| -x)(5)
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

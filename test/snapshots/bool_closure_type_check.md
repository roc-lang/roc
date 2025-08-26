# META
~~~ini
description=Boolean closure type checking - should have no errors
type=expr
~~~
# SOURCE
~~~roc
(|x| !x)(True)
~~~
# TOKENS
~~~text
OpenRound OpBar LowerIdent OpBar OpBang LowerIdent CloseRound OpenRound UpperIdent CloseRound ~~~
# PARSE
~~~clojure
(apply_anon
  (lambda
    (body
      (unary_not <unary>)
    )
    (args
      (lc "x")
    )
  )
  (uc "True")
)
~~~
# FORMATTED
~~~roc
\x -> !x(True)
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

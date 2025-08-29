# META
~~~ini
description=Tuple containing variations on boolean values
type=expr
~~~
# SOURCE
~~~roc
(True, False, Bool.True, Bool.False, !True, !False, True and False, !True or !True)
~~~
# TOKENS
~~~text
OpenRound UpperIdent Comma UpperIdent Comma UpperIdent Dot UpperIdent Comma UpperIdent Dot UpperIdent Comma OpBang UpperIdent Comma OpBang UpperIdent Comma UpperIdent OpAnd UpperIdent Comma OpBang UpperIdent OpOr OpBang UpperIdent CloseRound ~~~
# PARSE
~~~clojure
(tuple_literal
  (uc "True")
  (uc "False")
  (binop_pipe
    (uc "Bool")
    (uc "True")
  )
  (binop_pipe
    (uc "Bool")
    (uc "False")
  )
  (unary_not <unary>)
  (unary_not <unary>)
  (binop_and
    (uc "True")
    (uc "False")
  )
  (binop_or
    (unary_not <unary>)
    (unary_not <unary>)
  )
)
~~~
# FORMATTED
~~~roc
(True, False, Bool.True, Bool.False, !True, !False, True && False, !True || !True)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.tuple_literal)
~~~
# SOLVED
~~~clojure
(expr :tag tuple_literal :type "_a")
~~~
# TYPES
~~~roc
_a
~~~

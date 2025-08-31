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
  (unary_not <unary_op>)
  (unary_not <unary_op>)
  (binop_and
    (uc "True")
    (uc "False")
  )
  (binop_or
    (unary_not <unary_op>)
    (unary_not <unary_op>)
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
(Expr.tuple_literal
  (Expr.apply_tag)
  (Expr.apply_tag)
  (Expr.module_access
    (Expr.malformed)
    (Expr.malformed)
  )
  (Expr.module_access
    (Expr.malformed)
    (Expr.malformed)
  )
  (Expr.unary_not)
  (Expr.unary_not)
  (Expr.binop_and
    (Expr.apply_tag)
    (Expr.apply_tag)
  )
  (Expr.binop_or
    (Expr.unary_not)
    (Expr.unary_not)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag tuple_literal :type "_a")
~~~
# TYPES
~~~roc
_a
~~~

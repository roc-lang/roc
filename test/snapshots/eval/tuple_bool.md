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
  (binop_dot
    (uc "Bool")
    (uc "True")
  )
  (binop_dot
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
  (Expr.tag_no_args)
  (Expr.tag_no_args)
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
    (Expr.tag_no_args)
    (Expr.tag_no_args)
  )
  (Expr.binop_or
    (Expr.unary_not)
    (Expr.unary_not)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 25
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 -> #22)
(var #6 _)
(var #7 _)
(var #8 -> #23)
(var #9 _)
(var #10 -> #9)
(var #11 _)
(var #12 -> #11)
(var #13 -> #14)
(var #14 -> #15)
(var #15 _)
(var #16 -> #18)
(var #17 -> #18)
(var #18 -> #20)
(var #19 -> #18)
(var #20 _)
(var #21 -> #24)
(var #22 _)
(var #23 _)
(var #24 tuple)
~~~
# TYPES
~~~roc
~~~

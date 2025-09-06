# META
~~~ini
description=Block expression with lambda capture
type=expr
~~~
# SOURCE
~~~roc
{
    x = 42
    f = |y| x + y
    f(10)
}
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpAssign Int LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpPlus LowerIdent LowerIdent OpenRound Int CloseRound CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "x")
    (num_literal_i32 42)
  )
  (binop_equals
    (lc "f")
    (lambda
      (body
        (binop_plus
          (lc "x")
          (lc "y")
        )
      )
      (args
        (lc "y")
      )
    )
  )
  (apply_lc
    (lc "f")
    (num_literal_i32 10)
  )
)
~~~
# FORMATTED
~~~roc
x = 42
f = |y| x + y
f(10)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "x"))
    (Expr.num_literal_i32 42)
  )
  (Stmt.assign
    (pattern (Patt.ident "f"))
    (Expr.lambda (canonicalized))
  )
  (Expr.apply_ident)
)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
# No header found
~~~

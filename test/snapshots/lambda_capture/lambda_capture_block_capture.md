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
  (Expr.fn_call)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 18
(var #0 _)
(var #1 -> #2)
(var #2 Num *)
(var #3 _)
(var #4 -> #16)
(var #5 _)
(var #6 -> #7)
(var #7 -> #8)
(var #8 _)
(var #9 -> #16)
(var #10 _)
(var #11 -> #17)
(var #12 Num *)
(var #13 _)
(var #14 _)
(var #15 _)
(var #16 fn_pure)
(var #17 fn_pure)
~~~
# TYPES
~~~roc
x : Num(_size)
f : _arg -> _ret
y : _a
~~~

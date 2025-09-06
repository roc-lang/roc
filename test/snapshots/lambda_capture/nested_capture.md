# META
~~~ini
description="An inner lambda captures a variable defined in an outer lambda's scope."
type=expr
~~~
# SOURCE
~~~roc
{
    f = (|a| |b| a + b)
    g = f(10)
    g(5) # Expect: 15
}
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpAssign OpenRound OpBar LowerIdent OpBar OpBar LowerIdent OpBar LowerIdent OpPlus LowerIdent CloseRound LowerIdent OpAssign LowerIdent OpenRound Int CloseRound LowerIdent OpenRound Int CloseRound LineComment CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "f")
    (lambda
      (body
        (lambda
          (body
            (binop_plus
              (lc "a")
              (lc "b")
            )
          )
          (args
            (lc "b")
          )
        )
      )
      (args
        (lc "a")
      )
    )
  )
  (binop_equals
    (lc "g")
    (apply_lc
      (lc "f")
      (num_literal_i32 10)
    )
  )
  (apply_lc
    (lc "g")
    (num_literal_i32 5)
  )
)
~~~
# FORMATTED
~~~roc
f = |a| |b| a + b
g = f(10)
g(5)# Expect: 15
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "f"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "g"))
    (Expr.fn_call)
  )
  (Expr.fn_call)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 25
(var #0 _)
(var #1 -> #22)
(var #2 _)
(var #3 _)
(var #4 -> #5)
(var #5 -> #6)
(var #6 _)
(var #7 -> #21)
(var #8 -> #22)
(var #9 _)
(var #10 -> #13)
(var #11 -> #23)
(var #12 Num *)
(var #13 _)
(var #14 _)
(var #15 -> #24)
(var #16 Num *)
(var #17 _)
(var #18 _)
(var #19 _)
(var #20 _)
(var #21 fn_pure)
(var #22 fn_pure)
(var #23 fn_pure)
(var #24 fn_pure)
~~~
# TYPES
~~~roc
f : _arg -> _arg2 -> _ret
a : _c
b : _c
g : _c
~~~

# META
~~~ini
description=Dot access with proper variable definitions
type=expr
~~~
# SOURCE
~~~roc
{
    list = [1, 2, 3]
    fn = |x| x + 1
    list.map(fn)
}
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpAssign OpenSquare Int Comma Int Comma Int CloseSquare LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpPlus Int LowerIdent Dot LowerIdent OpenRound LowerIdent CloseRound CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "list")
    (list_literal
      (num_literal_i32 1)
      (num_literal_i32 2)
      (num_literal_i32 3)
    )
  )
  (binop_equals
    (lc "fn")
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
  )
  (apply_anon
    (binop_dot
      (lc "list")
      (dot_lc "map")
    )
    (lc "fn")
  )
)
~~~
# FORMATTED
~~~roc
list = [1, 2, 3]
fn = |x| x + 1
list..map(fn)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "list"))
    (Expr.list_literal)
  )
  (Stmt.assign
    (pattern (Patt.ident "fn"))
    (Expr.lambda (canonicalized))
  )
  (Expr.fn_call)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 23
(var #0 _)
(var #1 -> #5)
(var #2 Num *)
(var #3 Num *)
(var #4 Num *)
(var #5 _)
(var #6 _)
(var #7 -> #21)
(var #8 _)
(var #9 -> #10)
(var #10 -> #11)
(var #11 Num *)
(var #12 -> #21)
(var #13 _)
(var #14 _)
(var #15 _)
(var #16 -> #22)
(var #17 _)
(var #18 _)
(var #19 _)
(var #20 _)
(var #21 fn_pure)
(var #22 fn_pure)
~~~
# TYPES
~~~roc
x : _a
~~~

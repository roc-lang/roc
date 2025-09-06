# META
~~~ini
description=tuple_type
type=expr
~~~
# SOURCE
~~~roc
{
    f : (Str, Str) -> (Str, Str)
    f = |x| x

    f((1, 2))
}
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpColon OpenRound UpperIdent Comma UpperIdent CloseRound OpArrow OpenRound UpperIdent Comma UpperIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent BlankLine LowerIdent OpenRound OpenRound Int Comma Int CloseRound CloseRound CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "f")
    (binop_arrow_call
      (tuple_literal
        (uc "Str")
        (uc "Str")
      )
      (tuple_literal
        (uc "Str")
        (uc "Str")
      )
    )
  )
  (binop_equals
    (lc "f")
    (lambda
      (body
        (lc "x")
      )
      (args
        (lc "x")
      )
    )
  )
  (apply_lc
    (lc "f")
    (tuple_literal
      (num_literal_i32 1)
      (num_literal_i32 2)
    )
  )
)
~~~
# FORMATTED
~~~roc
f : (Str, Str) -> (Str, Str)
f = |x| x
f((1, 2))
~~~
# EXPECTED
TYPE MISMATCH - tuple_type.md:5:7:5:13
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "f"))
    (type type_8)
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
; Total type variables: 25
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 -> #22)
(var #11 _)
(var #12 _)
(var #13 -> #22)
(var #14 _)
(var #15 -> #24)
(var #16 Num *)
(var #17 Num *)
(var #18 -> #23)
(var #19 _)
(var #20 _)
(var #21 _)
(var #22 fn_pure)
(var #23 tuple)
(var #24 fn_pure)
~~~
# TYPES
~~~roc
f : _arg -> _ret
x : _a
~~~

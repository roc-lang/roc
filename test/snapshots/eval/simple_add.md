# META
~~~ini
description=Simple addition function with expect statement
type=file
~~~
# SOURCE
~~~roc
module [addU8]

addU8 : U8, U8 -> U8
addU8 = |a, b| a + b

expect addU8(1, 2) == 3
expect addU8(0, 10) == 10
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare BlankLine LowerIdent OpColon UpperIdent Comma UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar LowerIdent OpPlus LowerIdent BlankLine KwExpect LowerIdent OpenRound Int Comma Int CloseRound OpEquals Int KwExpect LowerIdent OpenRound Int Comma Int CloseRound OpEquals Int ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "addU8")
))
(block
  (binop_colon
    (lc "addU8")
    (binop_arrow_call
      (uc "U8")
      (binop_arrow_call
        (uc "U8")
        (uc "U8")
      )
    )
  )
  (binop_equals
    (lc "addU8")
    (lambda
      (body
        (binop_plus
          (lc "a")
          (lc "b")
        )
      )
      (args
        (lc "a")
        (lc "b")
      )
    )
  )
  (expect
    (binop_double_equals
      (apply_lc
        (lc "addU8")
        (tuple_literal
          (num_literal_i32 1)
          (num_literal_i32 2)
        )
      )
      (num_literal_i32 3)
    )
  )
  (expect
    (binop_double_equals
      (apply_lc
        (lc "addU8")
        (tuple_literal
          (num_literal_i32 0)
          (num_literal_i32 10)
        )
      )
      (num_literal_i32 10)
    )
  )
)
~~~
# FORMATTED
~~~roc
module [addU8]

addU8 : U8 -> U8 -> U8
addU8 = |a, b| a + b
expect addU8((1, 2)) == 3
expect addU8((0, 10)) == 10
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "addU8"))
    (type type_7)
  )
  (Stmt.assign
    (pattern (Patt.ident "addU8"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.expr)
  (Stmt.expr)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 39
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 -> #36)
(var #10 _)
(var #11 _)
(var #12 -> #13)
(var #13 -> #14)
(var #14 _)
(var #15 -> #36)
(var #16 _)
(var #17 _)
(var #18 Num *)
(var #19 Num *)
(var #20 _)
(var #21 _)
(var #22 Num *)
(var #23 _)
(var #24 _)
(var #25 _)
(var #26 Num *)
(var #27 Num *)
(var #28 _)
(var #29 _)
(var #30 Num *)
(var #31 _)
(var #32 _)
(var #33 _)
(var #34 _)
(var #35 _)
(var #36 fn_pure)
(var #37 _)
(var #38 _)
~~~
# TYPES
~~~roc
addU8 : _arg, _arg2 -> _ret
a : _c
b : _c
~~~

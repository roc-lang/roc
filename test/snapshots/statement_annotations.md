# META
~~~ini
description=Inline annotation for statements
type=file
~~~
# SOURCE
~~~roc
module []

addOneU64 = |x| {
  y : U64
  y = x + 1

  y
}

func : val -> val
func = |x| {
  y : val
  y = x

  y
}
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly LowerIdent OpColon UpperIdent LowerIdent OpAssign LowerIdent OpPlus Int BlankLine LowerIdent CloseCurly BlankLine LowerIdent OpColon LowerIdent OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly LowerIdent OpColon LowerIdent LowerIdent OpAssign LowerIdent BlankLine LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(module-header)
(block
  (binop_equals
    (lc "addOneU64")
    (lambda
      (body
        (block
          (binop_colon
            (lc "y")
            (uc "U64")
          )
          (binop_equals
            (lc "y")
            (binop_plus
              (lc "x")
              (num_literal_i32 1)
            )
          )
          (binop_colon
            (lc "y")
            (lc "y")
          )
        )
      )
      (args
        (lc "x")
      )
    )
  )
  (binop_colon
    (lc "func")
    (binop_arrow_call
      (lc "val")
      (lc "val")
    )
  )
  (binop_equals
    (lc "func")
    (lambda
      (body
        (block
          (binop_colon
            (lc "y")
            (lc "val")
          )
          (binop_equals
            (lc "y")
            (lc "x")
          )
          (binop_colon
            (lc "y")
            (lc "y")
          )
        )
      )
      (args
        (lc "x")
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
module []

addOneU64 = |x| {
	y : U64
	y = x + 1
	y : y
}

func : val -> val
func = |x| {
	y : val
	y = x
	y : y
}
~~~
# EXPECTED
TYPE MISMATCH - statement_annotations.md:13:7:13:8
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "addOneU64"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "func"))
    (type type_19)
  )
  (Stmt.assign
    (pattern (Patt.ident "func"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 39
(var #0 _)
(var #1 -> #36)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 -> #9)
(var #7 -> #8)
(var #8 -> #9)
(var #9 Num *)
(var #10 _)
(var #11 _)
(var #12 _)
(var #13 _)
(var #14 -> #36)
(var #15 _)
(var #16 _)
(var #17 _)
(var #18 _)
(var #19 _)
(var #20 _)
(var #21 -> #38)
(var #22 _)
(var #23 _)
(var #24 _)
(var #25 _)
(var #26 -> #27)
(var #27 _)
(var #28 _)
(var #29 _)
(var #30 _)
(var #31 _)
(var #32 -> #38)
(var #33 _)
(var #34 _)
(var #35 _)
(var #36 fn_pure)
(var #37 _)
(var #38 fn_pure)
~~~
# TYPES
~~~roc
x : _a
addOneU64 : _arg -> _ret
func : _arg -> _ret
y : _a
~~~

# META
~~~ini
description=Simple lambda constraint success test - verifies bidirectional type checking works correctly
type=file
~~~
# SOURCE
~~~roc
module [addTwo, addTwoF64]

# Should successfully constrain literal 2 to I64
addTwo : I64 -> I64
addTwo = |x| x + 2

# Should successfully constrain literal 2.0 to F64
addTwoF64 : F64 -> F64
addTwoF64 = |x| x + 2.0
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent Comma LowerIdent CloseSquare BlankLine LineComment LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpPlus Int BlankLine LineComment LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpPlus Float ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "addTwo")

    (lc "addTwoF64")
))
(block
  (binop_colon
    (lc "addTwo")
    (binop_arrow_call
      (uc "I64")
      (uc "I64")
    )
  )
  (binop_equals
    (lc "addTwo")
    (lambda
      (body
        (binop_plus
          (lc "x")
          (num_literal_i32 2)
        )
      )
      (args
        (lc "x")
      )
    )
  )
  (binop_colon
    (lc "addTwoF64")
    (binop_arrow_call
      (uc "F64")
      (uc "F64")
    )
  )
  (binop_equals
    (lc "addTwoF64")
    (lambda
      (body
        (binop_plus
          (lc "x")
          (frac_literal_small 2)
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
module [addTwo, addTwoF64]

# Should successfully constrain literal 2 to I64
addTwo : I64 -> I64
addTwo = |x| x + 2
# Should successfully constrain literal 2.0 to F64
addTwoF64 : F64 -> F64
addTwoF64 = |x| x + 2.0
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "addTwo"))
    (type type_6)
  )
  (Stmt.assign
    (pattern (Patt.ident "addTwo"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "addTwoF64"))
    (type type_18)
  )
  (Stmt.assign
    (pattern (Patt.ident "addTwoF64"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 32
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 -> #29)
(var #9 _)
(var #10 -> #11)
(var #11 -> #12)
(var #12 Num *)
(var #13 -> #29)
(var #14 _)
(var #15 _)
(var #16 _)
(var #17 _)
(var #18 _)
(var #19 _)
(var #20 -> #31)
(var #21 _)
(var #22 -> #23)
(var #23 -> #24)
(var #24 F64)
(var #25 -> #31)
(var #26 _)
(var #27 _)
(var #28 _)
(var #29 fn_pure)
(var #30 _)
(var #31 fn_pure)
~~~
# TYPES
~~~roc
x : _a
addTwo : _arg -> Num(_size)
addTwoF64 : _arg -> F64
~~~

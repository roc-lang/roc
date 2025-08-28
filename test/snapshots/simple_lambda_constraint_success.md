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
KwModule OpenSquare LowerIdent Comma LowerIdent CloseSquare LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpPlus Int LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpPlus Float ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "addTwo")
    (binop_thin_arrow
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
    (binop_thin_arrow
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

addTwo : I64 -> I64
addTwo = |x| x + 2
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
  (Expr.binop_colon
    (Expr.lookup "addTwo")
    (Expr.binop_thin_arrow
      (Expr.apply_tag)
      (Expr.apply_tag)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "addTwo")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.lookup "addTwoF64")
    (Expr.binop_thin_arrow
      (Expr.apply_tag)
      (Expr.apply_tag)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "addTwoF64")
    (Expr.lambda)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
addTwo : _a
addTwoF64 : _a
~~~

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
KwModule OpenSquare LowerIdent CloseSquare LowerIdent OpColon UpperIdent Comma UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar LowerIdent OpPlus LowerIdent KwExpect LowerIdent OpenRound Int Comma Int CloseRound OpEquals Int KwExpect LowerIdent OpenRound Int Comma Int CloseRound OpEquals Int ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "addU8")
    (binop_thin_arrow
      (uc "U8")
      (binop_thin_arrow
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
        (tuple_literal
          (lc "a")
          (lc "b")
        )
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
module [
	addU8,
]

addU8 : U8 -> U8 -> U8
addU8 = \(a, b) -> a + b
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
  (Expr.malformed)
  (Expr.malformed)
  (Expr.binop_minus)
  (Expr.binop_minus)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_c")
~~~
# TYPES
~~~roc
~~~

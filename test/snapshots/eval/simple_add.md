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
(module-header
  (exposes
    (lc "addU8")
))
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
  (Expr.binop_colon
    (Expr.lookup "addU8")
    (Expr.binop_thin_arrow
      (Expr.apply_tag)
      (Expr.binop_thin_arrow
        (Expr.apply_tag)
        (Expr.apply_tag)
      )
    )
  )
  (Expr.binop_equals
    (Expr.lookup "addU8")
    (Expr.lambda)
  )
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_c")
~~~
# TYPES
~~~roc
addU8 : _c
~~~

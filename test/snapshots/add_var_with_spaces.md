# META
~~~ini
description=Add a variable with spaces
type=file
~~~
# SOURCE
~~~roc
module [add2]

add2 = x +      2
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare BlankLine LowerIdent OpAssign LowerIdent OpPlus Int ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "add2")
))
~~~
# FORMATTED
~~~roc
module [add2]

add2 = x + 2
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_equals
    (Expr.lookup "add2")
    (Expr.binop_plus
      (Expr.lookup "x")
      (Expr.num_literal_i32 2)
    )
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
add2 : Num(_size)
~~~

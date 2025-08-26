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
KwModule OpenSquare LowerIdent CloseSquare LowerIdent OpAssign LowerIdent OpPlus Int ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "add2")
    (binop_plus
      (lc "x")
      (num_literal_i32 2)
    )
  )
)
~~~
# FORMATTED
~~~roc
module [
	add2,
]

add2 = x + 2
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~

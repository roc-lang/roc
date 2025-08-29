# META
~~~ini
description=Variable scoping with var keyword
type=file
~~~
# SOURCE
~~~roc
module []

# This should cause an error - var not allowed at top level
var topLevelVar_ = 0
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare KwVar LowerIdent OpAssign Int ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (var_lc "topLevelVar_")
    (num_literal_i32 0)
  )
)
~~~
# FORMATTED
~~~roc
module []

var topLevelVar_ = 0
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_equals
    (Expr.lookup "topLevelVar_")
    (Expr.num_literal_i32 0)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
topLevelVar_ : Num(_size)
~~~

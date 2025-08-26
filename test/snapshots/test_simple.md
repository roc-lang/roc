# META
~~~ini
description=Simple test for comment after equals
type=file
~~~
# SOURCE
~~~roc
module []

bar = # comment
    100
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare LowerIdent OpAssign Int ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "bar")
    (num_literal_i32 100)
  )
)
~~~
# FORMATTED
~~~roc
module []

bar =  # comment
	100
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

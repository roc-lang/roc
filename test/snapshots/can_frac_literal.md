# META
~~~ini
description=Float literal type inference
type=file
~~~
# SOURCE
~~~roc
module []

x = 3.14
y = 1.23e45
z = 0.5
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare LowerIdent OpAssign Float LowerIdent OpAssign Float LowerIdent OpAssign Float ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "x")
    (frac_literal_small 3.14)
  )
  (binop_equals
    (lc "y")
    (frac_literal_big big:<idx:0>)
  )
  (binop_equals
    (lc "z")
    (frac_literal_small 0.5)
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.record_access)
~~~
# SOLVED
~~~clojure
(expr :tag record_access :type "_a")
~~~
# TYPES
~~~roc
# File does not contain a block of statements
~~~

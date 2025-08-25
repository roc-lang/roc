# META
~~~ini
description=All fractional type annotations
type=file
~~~
# SOURCE
~~~roc
module []

a : F32
a = 3.14

b : F64
b = 2.71828

c : Dec
c = 123.456
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare LowerIdent OpColon UpperIdent LowerIdent OpAssign Float LowerIdent OpColon UpperIdent LowerIdent OpAssign Float LowerIdent OpColon UpperIdent LowerIdent OpAssign Float ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "a")
    (uc "F32")
  )
  (binop_equals
    (lc "a")
    (frac_literal_small 3.14)
  )
  (binop_colon
    (lc "b")
    (uc "F64")
  )
  (binop_equals
    (lc "b")
    (frac_literal_big big:<idx:0>)
  )
  (binop_colon
    (lc "c")
    (uc "Dec")
  )
  (binop_equals
    (lc "c")
    (frac_literal_big big:<idx:8>)
  )
)
~~~
# FORMATTED
~~~roc
module []

a: F32
a = 3.14

b: F64
b = 2.71828

c: Dec
c = 123.456
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "a")
    (Expr.apply_tag)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "b")
    (Expr.apply_tag)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "c")
    (Expr.apply_tag)
  )
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
~~~

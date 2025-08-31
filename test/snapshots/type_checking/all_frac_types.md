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
KwModule OpenSquare CloseSquare BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign Float BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign Float BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign Float ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
~~~roc
module []

a : F32
a = 3.14
b : F64
b = 2.71828
c : Dec
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
  (Expr.binop_equals
    (Expr.lookup "a")
    (Expr.frac_literal_small 3.14)
  )
  (Expr.binop_colon
    (Expr.lookup "b")
    (Expr.apply_tag)
  )
  (Expr.binop_equals
    (Expr.lookup "b")
    (Expr.frac_literal_big big:<idx:0>)
  )
  (Expr.binop_colon
    (Expr.lookup "c")
    (Expr.apply_tag)
  )
  (Expr.binop_equals
    (Expr.lookup "c")
    (Expr.frac_literal_big big:<idx:8>)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_d")
~~~
# TYPES
~~~roc
a : F64
b : F64
c : F64
~~~

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
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_d")
~~~
# TYPES
~~~roc
~~~

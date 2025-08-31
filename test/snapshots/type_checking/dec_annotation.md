# META
~~~ini
description=Dec type annotation
type=file
~~~
# SOURCE
~~~roc
module []

x : Dec
x = 123.456
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign Float ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
~~~roc
module []


x : Dec
x = 123.456
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
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~

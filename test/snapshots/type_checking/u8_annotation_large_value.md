# META
~~~ini
description=U8 type annotation with value exceeding U8 range
type=file
~~~
# SOURCE
~~~roc
module []

x : U8
x = 500
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign Int ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
~~~roc
module []


x : U8
x = 500
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

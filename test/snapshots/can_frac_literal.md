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
KwModule OpenSquare CloseSquare BlankLine LowerIdent OpAssign Float LowerIdent OpAssign Float LowerIdent OpAssign Float ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
~~~roc
module []


x = 3.14
y = 1.23e45
z = 0.5
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
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~

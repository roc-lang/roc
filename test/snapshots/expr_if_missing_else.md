# META
~~~ini
description=
type=file
~~~
# SOURCE
~~~roc
module []

foo = if tru 0
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare LowerIdent OpAssign KwIf LowerIdent Int ~~~
# PARSE
~~~clojure
(module-header)
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
(Expr.block
  (Expr.binop_equals
    (Expr.lookup "foo")
    (Expr.if_else)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
foo : _a
~~~

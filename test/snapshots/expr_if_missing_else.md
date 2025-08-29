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
(block
  (binop_equals
    (lc "foo")
    (if_without_else <0 branches>)
  )
)
~~~
# FORMATTED
~~~roc
module []

foo = if tru 0
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 3:7 to 3:14

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

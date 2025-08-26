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
NO CHANGE
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 3:7 to 3:14

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

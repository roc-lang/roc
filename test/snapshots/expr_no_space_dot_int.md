# META
~~~ini
description=
type=file
~~~
# SOURCE
~~~roc
module []

foo = asd.0
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine LowerIdent OpAssign LowerIdent Dot Int ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
~~~roc
module []


foo = (asd | 0)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
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

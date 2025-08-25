# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]0 f
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare Int LowerIdent ~~~
# PARSE
~~~clojure
(block
  (num_literal_i32 0)
  (lc "f")
)
~~~
# FORMATTED
~~~roc
module []

0
f
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.num_literal_i32 0)
  (Expr.lookup "f")
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~

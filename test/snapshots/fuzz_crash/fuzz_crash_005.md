# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
modu
~~~
# TOKENS
~~~text
LowerIdent ~~~
# PARSE
~~~clojure
(block
  (lc "modu")
)
~~~
# FORMATTED
~~~roc
modu
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.lookup "modu")
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~

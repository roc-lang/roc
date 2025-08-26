# META
~~~ini
description=Block with single expression
type=expr
~~~
# SOURCE
~~~roc
{ x }
~~~
# TOKENS
~~~text
OpenCurly LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(block
  (lc "x")
)
~~~
# FORMATTED
~~~roc
x
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.str_literal_big)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~

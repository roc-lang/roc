# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
F
~~~
# TOKENS
~~~text
UpperIdent ~~~
# PARSE
~~~clojure
(block
  (uc "F")
)
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
  (Expr.str_literal_small)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~

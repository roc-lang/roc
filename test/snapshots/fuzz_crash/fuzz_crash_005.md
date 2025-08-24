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
NO CHANGE
~~~
# EXPECTED
MISSING HEADER - fuzz_crash_005.md:1:1:1:5
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

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
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.record_access)
~~~
# SOLVED
~~~clojure
(expr :tag record_access :type "_a")
~~~
# TYPES
~~~roc
# File does not contain a block of statements
~~~

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

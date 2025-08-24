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
MISSING HEADER - fuzz_crash_004.md:1:1:1:2
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.apply_tag)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "[]_others")
~~~
# TYPES
~~~roc
~~~

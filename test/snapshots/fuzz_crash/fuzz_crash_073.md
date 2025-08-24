# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]!0.t
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare OpBang Int Dot LowerIdent ~~~
# PARSE
~~~clojure
(block
  (unary_not <unary>)
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
ASCII CONTROL CHARACTER - :0:0:0:0
PARSE ERROR - fuzz_crash_073.md:1:9:1:10
PARSE ERROR - fuzz_crash_073.md:1:10:1:11
PARSE ERROR - fuzz_crash_073.md:1:12:1:14
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
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
~~~

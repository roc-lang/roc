# META
~~~ini
description=Double question default value
type=expr
~~~
# SOURCE
~~~roc
get_name!({}) ?? "Bob"
~~~
# TOKENS
~~~text
LowerIdent OpBang OpenRound OpenCurly CloseCurly CloseRound OpDoubleQuestion String ~~~
# PARSE
~~~clojure
(lc "get_name")
~~~
# FORMATTED
~~~roc
get_name
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.lookup "get_name")
~~~
# SOLVED
~~~clojure
(expr :tag lookup :type "_a")
~~~
# TYPES
~~~roc
_a
~~~

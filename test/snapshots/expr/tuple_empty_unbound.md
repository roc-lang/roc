# META
~~~ini
description=Empty tuple literal
type=expr
~~~
# SOURCE
~~~roc
()
~~~
# TOKENS
~~~text
OpenRound CloseRound ~~~
# PARSE
~~~clojure
(tuple_literal)
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
(Expr.tuple_literal
)
~~~
# SOLVED
~~~clojure
(expr :tag tuple_literal :type "_a")
~~~
# TYPES
~~~roc
_a
~~~

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
(Expr.binop_double_slash)
~~~
# SOLVED
~~~clojure
(expr :tag binop_double_slash :type "_a")
~~~
# TYPES
~~~roc
_a
~~~

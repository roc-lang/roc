# META
~~~ini
description=not_tag
type=expr
~~~
# SOURCE
~~~roc
!(C(2))
~~~
# TOKENS
~~~text
OpBang OpenRound UpperIdent OpenRound Int CloseRound CloseRound ~~~
# PARSE
~~~clojure
(unary_not <unary>)
~~~
# FORMATTED
~~~roc
!C(2)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.unary_double_dot)
~~~
# SOLVED
~~~clojure
(expr :tag unary_double_dot :type "_a")
~~~
# TYPES
~~~roc
_a
~~~

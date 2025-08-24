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
NO CHANGE
~~~
# EXPECTED
TYPE MISMATCH - not_tag.md:1:1:1:8
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.unary_not)
~~~
# SOLVED
~~~clojure
(expr :tag unary_not :type "[True, False]_others")
~~~
# TYPES
~~~roc
[True, False]_others
~~~

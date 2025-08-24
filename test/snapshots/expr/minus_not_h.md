# META
~~~ini
description=Unary minus and boolean not (should error)
type=expr
~~~
# SOURCE
~~~roc
-!h
~~~
# TOKENS
~~~text
OpUnaryMinus OpBang LowerIdent ~~~
# PARSE
~~~clojure
(unary_neg <unary>)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - minus_not_h.md:1:1:1:2
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.unary_neg)
~~~
# SOLVED
~~~clojure
(expr :tag unary_neg :type "[True, False]_others")
~~~
# TYPES
~~~roc
[True, False]_others
~~~

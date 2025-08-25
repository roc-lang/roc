# META
~~~ini
description=if_then_else (5)
type=expr
~~~
# SOURCE
~~~roc
if bool { # Comment after then open
	A # Comment after expr
} else B
~~~
# TOKENS
~~~text
KwIf LowerIdent OpenCurly UpperIdent CloseCurly KwElse UpperIdent ~~~
# PARSE
~~~clojure
(if_else <0 branches>)
~~~
# FORMATTED
~~~roc
if bool {
	A # Comment after expr
} else B
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 1:9

# CANONICALIZE
~~~clojure
(Expr.if_else)
~~~
# SOLVED
~~~clojure
(expr :tag if_else :type "[]_others")
~~~
# TYPES
~~~roc
[]_others
~~~

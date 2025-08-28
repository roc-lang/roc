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
if bool
	{
		 # Comment after then open
A # Comment after expr
	}
else B
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 1:9

# CANONICALIZE
~~~clojure
(Expr.match)
~~~
# SOLVED
~~~clojure
(expr :tag match :type "_a")
~~~
# TYPES
~~~roc
_a
~~~

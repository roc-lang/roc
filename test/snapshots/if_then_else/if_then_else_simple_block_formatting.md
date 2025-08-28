# META
~~~ini
description=if_then_else (3)
type=expr
~~~
# SOURCE
~~~roc
if bool {
	A
} else 2
~~~
# TOKENS
~~~text
KwIf LowerIdent OpenCurly UpperIdent CloseCurly KwElse Int ~~~
# PARSE
~~~clojure
(if_else <0 branches>)
~~~
# FORMATTED
~~~roc
if bool
	{
		A
	}
else 2
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
(expr :tag if_else :type "_a")
~~~
# TYPES
~~~roc
_a
~~~

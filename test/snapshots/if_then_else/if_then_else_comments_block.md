# META
~~~ini
description=if_then_else (11)
type=expr
~~~
# SOURCE
~~~roc
if # Comment after if
	bool
		{
			1
		} else {
			2
		}
~~~
# TOKENS
~~~text
KwIf LowerIdent OpenCurly Int CloseCurly KwElse OpenCurly Int CloseCurly ~~~
# PARSE
~~~clojure
(if_else <0 branches>)
~~~
# FORMATTED
~~~roc
if # Comment after if
bool
	{
		1
	}
else {
	2
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 3:3

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

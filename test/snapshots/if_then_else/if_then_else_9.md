# META
~~~ini
description=if_then_else (9)
type=expr
~~~
# SOURCE
~~~roc
if bool {
	1
} else if 10 { # Comment after else open
	A
} else { # Comment after else open
	3
}
~~~
# TOKENS
~~~text
KwIf LowerIdent OpenCurly Int CloseCurly KwElse KwIf Int OpenCurly UpperIdent CloseCurly KwElse OpenCurly Int CloseCurly ~~~
# PARSE
~~~clojure
(if_else <3 branches>)
~~~
# FORMATTED
~~~roc
if bool
	{
		1
	}
else if 10
	{
		A
	}
else {
	3
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 1:9

**Parse Error**
at 3:8 to 3:14

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

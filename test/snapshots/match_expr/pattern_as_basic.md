# META
~~~ini
description=Basic as pattern to bind both pattern and whole value
type=expr
~~~
# SOURCE
~~~roc
match (1, 2) {
    (x, y) as point => point
}
~~~
# TOKENS
~~~text
KwMatch OpenRound Int Comma Int CloseRound OpenCurly OpenRound LowerIdent Comma LowerIdent CloseRound KwAs LowerIdent OpFatArrow LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(match <9 branches>)
~~~
# FORMATTED
~~~roc
when (1, 2) is {
	(x, y)
	<malformed>
	point
	<malformed>
	point
} -> <malformed>
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 1:14

**Parse Error**
at 2:12 to 2:12

**Parse Error**
at 2:21 to 2:21

**Parse Error**
at 1:1 to 3:2

**Parse Error**
at 3:2 to 3:2

**Unsupported Node**
at 1:12 to 1:13

**Unsupported Node**
at 1:14 to 3:1

**Unsupported Node**
at 3:2 to 3:2

# CANONICALIZE
~~~clojure
(Expr.match)
~~~
# SOLVED
~~~clojure
(expr :tag match :type "Error")
~~~
# TYPES
~~~roc
Error
~~~

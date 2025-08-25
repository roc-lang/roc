# META
~~~ini
description=Match expression with invalid (old style) list rest patterns should error
type=expr
~~~
# SOURCE
~~~roc
match items {
    [first, ..rest] => 0 # invalid rest pattern should error
    [..rest, last] => 1 # invalid rest pattern should error
    [x, ..rest, y] => 2 # invalid rest pattern should error
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenSquare LowerIdent Comma DoubleDot LowerIdent CloseSquare OpFatArrow Int OpenSquare DoubleDot LowerIdent Comma LowerIdent CloseSquare OpFatArrow Int OpenSquare LowerIdent Comma DoubleDot LowerIdent Comma LowerIdent CloseSquare OpFatArrow Int CloseCurly ~~~
# PARSE
~~~clojure
(match <16 branches>)
~~~
# FORMATTED
~~~roc
when items is {
	[(first, <unary_double_dot>)]
	<malformed>
	0
	[(<unary_double_dot>, last)]
	<malformed>
	1
	[(x, <unary_double_dot>, y)]
	<malformed>
	2
} -> <malformed>
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 1:13

**Parse Error**
at 2:21 to 2:21

**Parse Error**
at 3:20 to 3:20

**Parse Error**
at 4:20 to 4:20

**Parse Error**
at 1:1 to 5:2

**Parse Error**
at 5:2 to 5:2

**Unsupported Node**
at 1:13 to 4:25

**Unsupported Node**
at 5:2 to 5:2

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

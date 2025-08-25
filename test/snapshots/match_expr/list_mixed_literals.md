# META
~~~ini
description=Match expression with mixed literal and variable patterns in lists
type=expr
~~~
# SOURCE
~~~roc
match sequence {
    [0, count] => count
    [1, x, 3] => x
    [42, value] => value
    [first, 99] => first
    [] => 0
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenSquare Int Comma LowerIdent CloseSquare OpFatArrow LowerIdent OpenSquare Int Comma LowerIdent Comma Int CloseSquare OpFatArrow LowerIdent OpenSquare Int Comma LowerIdent CloseSquare OpFatArrow LowerIdent OpenSquare LowerIdent Comma Int CloseSquare OpFatArrow LowerIdent OpenSquare CloseSquare OpFatArrow Int CloseCurly ~~~
# PARSE
~~~clojure
(match <24 branches>)
~~~
# FORMATTED
~~~roc
when sequence is {
	[0, count]
	=>
	count
	[1, x, 3]
	=>
	x
	[42, value]
	=>
	value
	[first, 99]
	=>
	first
	[]
	=>
	0
} -> 
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 1:16

**Parse Error**
at 2:16 to 2:16

**Parse Error**
at 3:15 to 3:15

**Parse Error**
at 4:17 to 4:17

**Parse Error**
at 5:17 to 5:17

**Parse Error**
at 6:8 to 6:8

**Parse Error**
at 1:1 to 7:2

**Parse Error**
at 7:2 to 7:2

**Unsupported Node**
at 1:16 to 7:1

**Unsupported Node**
at 7:2 to 7:2

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

# META
~~~ini
description=List rest patterns with proper variable scoping across branches
type=expr
~~~
# SOURCE
~~~roc
match data {
    [..items] => 1
    [first, ..items] => first
    [..items, last] => last
    [first, ..items, last] => first + last
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenSquare DoubleDot LowerIdent CloseSquare OpFatArrow Int OpenSquare LowerIdent Comma DoubleDot LowerIdent CloseSquare OpFatArrow LowerIdent OpenSquare DoubleDot LowerIdent Comma LowerIdent CloseSquare OpFatArrow LowerIdent OpenSquare LowerIdent Comma DoubleDot LowerIdent Comma LowerIdent CloseSquare OpFatArrow LowerIdent OpPlus LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(match <21 branches>)
~~~
# FORMATTED
~~~roc
when data is {
	[<unary_double_dot>]
	<malformed>
	1
	[(first, <unary_double_dot>)]
	<malformed>
	first
	[(<unary_double_dot>, last)]
	<malformed>
	last
	[(first, <unary_double_dot>, last)]
	<malformed>
	first + last
} -> <malformed>
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 1:12

**Parse Error**
at 2:15 to 2:15

**Parse Error**
at 3:22 to 3:22

**Parse Error**
at 4:21 to 4:21

**Parse Error**
at 5:28 to 5:28

**Parse Error**
at 1:1 to 6:2

**Parse Error**
at 6:2 to 6:2

**Unsupported Node**
at 1:12 to 6:1

**Unsupported Node**
at 6:2 to 6:2

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

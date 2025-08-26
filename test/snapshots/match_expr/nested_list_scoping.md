# META
~~~ini
description=Match expression with nested list patterns testing variable scoping
type=expr
~~~
# SOURCE
~~~roc
match nestedList {
    [[x], [y]] => x + y
    [[x, y]] => x - y  
    [x, [y]] => x * y
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenSquare OpenSquare LowerIdent CloseSquare Comma OpenSquare LowerIdent CloseSquare CloseSquare OpFatArrow LowerIdent OpPlus LowerIdent OpenSquare OpenSquare LowerIdent Comma LowerIdent CloseSquare CloseSquare OpFatArrow LowerIdent OpBinaryMinus LowerIdent OpenSquare LowerIdent Comma OpenSquare LowerIdent CloseSquare CloseSquare OpFatArrow LowerIdent OpStar LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(match <21 branches>)
~~~
# FORMATTED
~~~roc
when nestedList is {
	[[x], [y]]
	=>
	x + y
	[[x, y]]
	=>
	x - y
	[x, [y]]
	=>
	x * y
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 1:18

**Parse Error**
at 2:16 to 2:16

**Parse Error**
at 3:14 to 3:14

**Parse Error**
at 4:14 to 4:14

**Parse Error**
at 1:1 to 5:2

**Parse Error**
at 5:2 to 5:2

# CANONICALIZE
~~~clojure
(Expr.dot_num)
~~~
# SOLVED
~~~clojure
(expr :tag dot_num :type "_a")
~~~
# TYPES
~~~roc
_a
~~~

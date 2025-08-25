# META
~~~ini
description=Match expression with wrong arrow
type=expr
~~~
# SOURCE
~~~roc
match l {
    [] -> Err(EmptyList)
    [.., e] -> Ok(e)
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenSquare CloseSquare OpArrow UpperIdent OpenRound UpperIdent CloseRound OpenSquare DoubleDot Comma LowerIdent CloseSquare OpArrow UpperIdent OpenRound LowerIdent CloseRound CloseCurly ~~~
# PARSE
~~~clojure
(match <12 branches>)
~~~
# FORMATTED
~~~roc
when l is {
	[]
	<malformed>
	Err(EmptyList)
	[<unary_double_dot>]
	e
	<malformed>
	<malformed>
	Ok(e)
} -> <malformed>
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 1:9

**Parse Error**
at 2:8 to 2:8

**Parse Error**
at 3:8 to 3:8

**Parse Error**
at 3:5 to 3:10

**Parse Error**
at 3:11 to 3:11

**Parse Error**
at 3:13 to 3:13

**Parse Error**
at 1:1 to 4:2

**Parse Error**
at 4:2 to 4:2

**Unsupported Node**
at 1:9 to 4:1

**Unsupported Node**
at 4:2 to 4:2

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

# META
~~~ini
description=Match expression with tag patterns containing payloads
type=expr
~~~
# SOURCE
~~~roc
match shape {
    Circle(radius) => 3.14 * radius * radius
    Rectangle(width, height) => width * height
    Triangle(base, height) => 0.5 * base * height
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly UpperIdent OpenRound LowerIdent CloseRound OpFatArrow Float OpStar LowerIdent OpStar LowerIdent UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpFatArrow LowerIdent OpStar LowerIdent UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpFatArrow Float OpStar LowerIdent OpStar LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(match <29 branches>)
~~~
# FORMATTED
~~~roc
when shape is {
	Circle(radius)
	=>
	(3.14 * radius) * radius
	Rectangle((width, height))
	=>
	width * height
	Triangle((base, height))
	=>
	(0.5 * base) * height
} -> 
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 1:13

**Parse Error**
at 2:20 to 2:20

**Parse Error**
at 3:30 to 3:30

**Parse Error**
at 4:28 to 4:28

**Parse Error**
at 1:1 to 5:2

**Parse Error**
at 5:2 to 5:2

**Unsupported Node**
at 1:13 to 5:1

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

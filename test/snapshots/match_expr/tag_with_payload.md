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
(match <0 branches>)
~~~
# FORMATTED
~~~roc
match shape
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 2:20 to 2:23

**Parse Error**
at 3:30 to 3:33

**Parse Error**
at 4:28 to 4:31

**Parse Error**
at 1:13 to 5:2

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

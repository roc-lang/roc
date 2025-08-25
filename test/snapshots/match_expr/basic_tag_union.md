# META
~~~ini
description=Basic tag union match with simple patterns
type=expr
~~~
# SOURCE
~~~roc
match color {
	Red => 1
	Blue => 2
	Green => "3"
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly UpperIdent OpFatArrow Int UpperIdent OpFatArrow Int UpperIdent OpFatArrow String CloseCurly ~~~
# PARSE
~~~clojure
(match <9 branches>)
~~~
# FORMATTED
~~~roc
when color is {
	Red
	<malformed>
	1
	Blue
	<malformed>
	2
	Green
	<malformed>
	"3"
} -> <malformed>
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 1:13

**Parse Error**
at 2:6 to 2:6

**Parse Error**
at 3:7 to 3:7

**Parse Error**
at 4:8 to 4:8

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

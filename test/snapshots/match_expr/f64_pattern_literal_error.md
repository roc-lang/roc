# META
~~~ini
description=Match expression with f64 literal pattern (should error)
type=expr
~~~
# SOURCE
~~~roc
match x {
    3.14f64 => "pi"
    0.0f64 => "zero"
    value => "other"
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly Float LowerIdent OpFatArrow String Float LowerIdent OpFatArrow String LowerIdent OpFatArrow String CloseCurly ~~~
# PARSE
~~~clojure
(match <11 branches>)
~~~
# FORMATTED
~~~roc
when x is {
	3.14f64
	f64
	=>
	"pi"
	0.0f64
	f64
	=>
	"zero"
	value
	=>
	"other"
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 1:9

**Parse Error**
at 2:13 to 2:13

**Parse Error**
at 3:12 to 3:12

**Parse Error**
at 4:11 to 4:11

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

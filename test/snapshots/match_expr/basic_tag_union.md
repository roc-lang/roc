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
(match <0 branches>)
~~~
# FORMATTED
~~~roc
match color
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 2:6 to 2:9

**Parse Error**
at 3:7 to 3:10

**Parse Error**
at 4:8 to 4:11

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

# META
~~~ini
description=Match expression with multiple patterns in one branch
type=expr
~~~
# SOURCE
~~~roc
match color {
    Blue | Green | Red => 1
    Black => 2
    White => 3
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly UpperIdent OpBar UpperIdent OpBar UpperIdent OpFatArrow Int UpperIdent OpFatArrow Int UpperIdent OpFatArrow Int CloseCurly ~~~
# PARSE
~~~clojure
(match <13 branches>)
~~~
# FORMATTED
~~~roc
when color is {
	Blue.Green | Red
	=>
	1
	Black
	=>
	2
	White
	=>
	3
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 1:13

**Parse Error**
at 2:24 to 2:24

**Parse Error**
at 3:11 to 3:11

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

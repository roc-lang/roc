# META
~~~ini
description=Match expression with guard conditions using if clauses
type=expr
~~~
# SOURCE
~~~roc
match value {
    x if x > 0 => "positive: ${Num.toStr x}"
    x if x < 0 => "negative: ${Num.toStr x}"
    _ => "other"
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly LowerIdent KwIf LowerIdent OpGreaterThan Int OpFatArrow String LowerIdent KwIf LowerIdent OpLessThan Int OpFatArrow String Underscore OpFatArrow String CloseCurly ~~~
# PARSE
~~~clojure
(match <17 branches>)
~~~
# FORMATTED
~~~roc
when value is {
	x
	if x > 0 => "positive: ${Num.toStr x}" x
	if x < 0 => "negative: ${Num.toStr x}" _ => "other"
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 1:13

**Parse Error**
at 2:7 to 3:5

**Parse Error**
at 3:7 to 4:5

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

# META
~~~ini
description=Match expression with boolean-like tag patterns
type=expr
~~~
# SOURCE
~~~roc
match isReady {
	True => "ready to go!"
	False => "not ready yet"
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly UpperIdent OpFatArrow String UpperIdent OpFatArrow String CloseCurly ~~~
# PARSE
~~~clojure
(match <6 branches>)
~~~
# FORMATTED
~~~roc
when isReady is {
	True
	=>
	"ready to go!"
	False
	=>
	"not ready yet"
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 1:15

**Parse Error**
at 2:7 to 2:7

**Parse Error**
at 3:8 to 3:8

**Parse Error**
at 1:1 to 4:2

**Parse Error**
at 4:2 to 4:2

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

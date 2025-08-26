# META
~~~ini
description=Match expression with tag patterns and variable catch-all pattern
type=expr
~~~
# SOURCE
~~~roc
match value {
    Answer => "the answer"
    Zero => "zero"
    other => "something else"
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly UpperIdent OpFatArrow String UpperIdent OpFatArrow String LowerIdent OpFatArrow String CloseCurly ~~~
# PARSE
~~~clojure
(match <9 branches>)
~~~
# FORMATTED
~~~roc
when value is {
	Answer
	=>
	"the answer"
	Zero
	=>
	"zero"
	other
	=>
	"something else"
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 1:13

**Parse Error**
at 2:12 to 2:12

**Parse Error**
at 3:10 to 3:10

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

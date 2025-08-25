# META
~~~ini
description=Match expression with single branch (simple variable pattern)
type=expr
~~~
# SOURCE
~~~roc
match value {
    x => x + 1
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly LowerIdent OpFatArrow LowerIdent OpPlus Int CloseCurly ~~~
# PARSE
~~~clojure
(match <5 branches>)
~~~
# FORMATTED
~~~roc
when value is {
	x
	<malformed>
	x + 1
} -> <malformed>
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 1:13

**Parse Error**
at 2:7 to 2:7

**Parse Error**
at 1:1 to 3:2

**Parse Error**
at 3:2 to 3:2

**Unsupported Node**
at 1:13 to 3:1

**Unsupported Node**
at 3:2 to 3:2

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

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
(match
  (scrutinee     (lc "isReady")
))
~~~
# FORMATTED
~~~roc
match isReady
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 2:7 to 2:7

**Parse Error**
at 3:8 to 3:8

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

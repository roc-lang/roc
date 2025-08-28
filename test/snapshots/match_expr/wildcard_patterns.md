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
(match
  (scrutinee     (lc "value")
))
~~~
# FORMATTED
~~~roc
match value
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 2:12 to 2:12

**Parse Error**
at 3:10 to 3:10

**Parse Error**
at 4:11 to 4:11

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

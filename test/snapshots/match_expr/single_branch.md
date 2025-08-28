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
at 2:7 to 2:7

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

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
(match
  (scrutinee     (lc "x")
))
~~~
# FORMATTED
~~~roc
match x
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 2:13 to 2:13

**Parse Error**
at 3:12 to 3:12

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

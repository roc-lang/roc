# META
~~~ini
description=Match expression with fractional literals that exceed Dec precision
type=expr
~~~
# SOURCE
~~~roc
match x {
    1e100 => "very large number"
    1e-40 => "very small number"
    1.7976931348623157e308 => "near f64 max"
    0.0 => "zero"
    value => "other"
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly Float OpFatArrow String Float OpFatArrow String Float OpFatArrow String Float OpFatArrow String LowerIdent OpFatArrow String CloseCurly ~~~
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
at 2:11 to 2:14

**Parse Error**
at 3:11 to 3:14

**Parse Error**
at 4:28 to 4:31

**Parse Error**
at 5:9 to 5:12

**Parse Error**
at 6:11 to 6:14

**Parse Error**
at 1:9 to 7:2

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

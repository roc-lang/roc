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
)
  (branch1     (binop_thick_arrow
      (lc "x")
      (binop_plus
        (lc "x")
        (num_literal_i32 1)
      )
    )
))
~~~
# FORMATTED
~~~roc
match value
	x => x + 1
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 2:7 to 2:9

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

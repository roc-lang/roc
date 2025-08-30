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
)
  (branch1     (binop_thick_arrow
      (uc "Answer")
      (str_literal_big "the answer")
    )
)
  (branch2     (binop_thick_arrow
      (uc "Zero")
      (str_literal_small "zero")
    )
)
  (branch3     (binop_thick_arrow
      (lc "other")
      (str_literal_big "something else")
    )
))
~~~
# FORMATTED
~~~roc
match value
	Answer => "the answer"
	Zero => "zero"
	other => "something else"
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 2:12 to 2:14

**Unsupported Node**
at 3:5 to 3:9

**Unsupported Node**
at 4:11 to 4:13

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

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
)
  (branch1     (binop_thick_arrow
      (uc "True")
      (str_literal_big "ready to go!")
    )
)
  (branch2     (binop_thick_arrow
      (uc "False")
      (str_literal_big "not ready yet")
    )
))
~~~
# FORMATTED
~~~roc
match isReady
	True => "ready to go!"
	False => "not ready yet"
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 2:7 to 2:9

**Unsupported Node**
at 3:2 to 3:7

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

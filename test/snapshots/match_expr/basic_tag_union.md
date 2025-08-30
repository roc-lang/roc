# META
~~~ini
description=Basic tag union match with simple patterns
type=expr
~~~
# SOURCE
~~~roc
match color {
	Red => 1
	Blue => 2
	Green => "3"
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly UpperIdent OpFatArrow Int UpperIdent OpFatArrow Int UpperIdent OpFatArrow String CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (lc "color")
)
  (branch1     (binop_thick_arrow
      (uc "Red")
      (num_literal_i32 1)
    )
)
  (branch2     (binop_thick_arrow
      (uc "Blue")
      (num_literal_i32 2)
    )
)
  (branch3     (binop_thick_arrow
      (uc "Green")
      (str_literal_small "3")
    )
))
~~~
# FORMATTED
~~~roc
match color
	Red => 1
	Blue => 2
	Green => "3"
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 2:6 to 2:8

**Unsupported Node**
at 3:2 to 3:6

**Unsupported Node**
at 4:8 to 4:10

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

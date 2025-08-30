# META
~~~ini
description=Match expression with tag patterns containing payloads
type=expr
~~~
# SOURCE
~~~roc
match shape {
    Circle(radius) => 3.14 * radius * radius
    Rectangle(width, height) => width * height
    Triangle(base, height) => 0.5 * base * height
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly UpperIdent OpenRound LowerIdent CloseRound OpFatArrow Float OpStar LowerIdent OpStar LowerIdent UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpFatArrow LowerIdent OpStar LowerIdent UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpFatArrow Float OpStar LowerIdent OpStar LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (lc "shape")
)
  (branch1     (binop_thick_arrow
      (apply_uc
        (uc "Circle")
        (lc "radius")
      )
      (binop_star
        (binop_star
          (frac_literal_small 3.14)
          (lc "radius")
        )
        (lc "radius")
      )
    )
)
  (branch2     (binop_thick_arrow
      (apply_uc
        (uc "Rectangle")
        (tuple_literal
          (lc "width")
          (lc "height")
        )
      )
      (binop_star
        (lc "width")
        (lc "height")
      )
    )
)
  (branch3     (binop_thick_arrow
      (apply_uc
        (uc "Triangle")
        (tuple_literal
          (lc "base")
          (lc "height")
        )
      )
      (binop_star
        (binop_star
          (frac_literal_small 0.5)
          (lc "base")
        )
        (lc "height")
      )
    )
))
~~~
# FORMATTED
~~~roc
match shape
	Circle(radius) => (3.14 * radius) * radius
	Rectangle(width, height) => width * height
	Triangle(base, height) => (0.5 * base) * height
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 2:20 to 2:22

**Unsupported Node**
at 3:5 to 3:29

**Unsupported Node**
at 4:28 to 4:30

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

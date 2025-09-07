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
        (lc "width")
        (lc "height")
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
        (lc "base")
        (lc "height")
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
UNDEFINED VARIABLE - tag_with_payload.md:1:7:1:12
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **shape** in this scope.
Is there an **import** or **exposing** missing up-top?

**tag_with_payload.md:1:7:1:12:**
```roc
match shape {
```
      ^^^^^


**SHADOWING**
This definition shadows an existing one.

**tag_with_payload.md:4:20:4:26:**
```roc
    Triangle(base, height) => 0.5 * base * height
```
                   ^^^^^^


# CANONICALIZE
~~~clojure
(Expr.match)
~~~
# SOLVED
~~~clojure
; Total type variables: 30
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 F64)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 _)
(var #11 _)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 _)
(var #16 _)
(var #17 _)
(var #18 _)
(var #19 _)
(var #20 _)
(var #21 _)
(var #22 _)
(var #23 F64)
(var #24 _)
(var #25 _)
(var #26 _)
(var #27 _)
(var #28 _)
(var #29 _)
~~~
# TYPES
~~~roc
height : _a
width : _a
radius : _a
base : _a
~~~

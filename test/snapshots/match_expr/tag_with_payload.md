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
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **shape** in this scope.
Is there an **import** or **exposing** missing up-top?

**tag_with_payload.md:1:7:1:12:**
```roc
match shape {
```
      ^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**tag_with_payload.md:2:5:2:45:**
```roc
    Circle(radius) => 3.14 * radius * radius
```
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**tag_with_payload.md:4:5:4:50:**
```roc
    Triangle(base, height) => 0.5 * base * height
```
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


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

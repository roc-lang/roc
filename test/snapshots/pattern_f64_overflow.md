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
)
  (branch1     (binop_thick_arrow
      (frac_literal_big big:<idx:95>)
      (block
        (str_literal_big "very large number")
        (binop_thick_arrow
          (frac_literal_big big:<idx:119>)
          (str_literal_big "very small number")
        )
        (binop_thick_arrow
          (frac_literal_big big:<idx:143>)
          (str_literal_big "near f64 max")
        )
        (binop_thick_arrow
          (frac_literal_small 0)
          (str_literal_small "zero")
        )
        (binop_thick_arrow
          (lc "value")
          (str_literal_big "other")
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
match x
	1e100 => 
		"very large number"
		1e-40 => "very small number"
		1.7976931348623157e308 => "near f64 max"
		0.0 => "zero"
		value => "other"
~~~
# EXPECTED
UNDEFINED VARIABLE - pattern_f64_overflow.md:1:7:1:8
F64 NOT ALLOWED IN PATTERN - :0:0:0:0
F64 NOT ALLOWED IN PATTERN - :0:0:0:0
F64 NOT ALLOWED IN PATTERN - :0:0:0:0
UNUSED VARIABLE - pattern_f64_overflow.md:6:5:6:10
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **x** in this scope.
Is there an **import** or **exposing** missing up-top?

**pattern_f64_overflow.md:1:7:1:8:**
```roc
match x {
```
      ^


# CANONICALIZE
~~~clojure
(Expr.match)
~~~
# SOLVED
~~~clojure
; Total type variables: 19
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 Str)
(var #4 _)
(var #5 Str)
(var #6 _)
(var #7 _)
(var #8 Str)
(var #9 _)
(var #10 _)
(var #11 Str)
(var #12 _)
(var #13 _)
(var #14 Str)
(var #15 _)
(var #16 _)
(var #17 _)
(var #18 _)
~~~
# TYPES
~~~roc
value : _a
~~~

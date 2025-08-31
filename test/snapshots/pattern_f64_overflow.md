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
      (frac_literal_big big:very large number)
      (block
        (str_literal_big "very large number")
        (binop_thick_arrow
          (frac_literal_big big:<invalid:24>)
          (str_literal_big "very small number")
        )
        (binop_thick_arrow
          (frac_literal_big big:<invalid:48>)
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
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**pattern_f64_overflow.md:2:5:6:21:**
```roc
    1e100 => "very large number"
    1e-40 => "very small number"
    1.7976931348623157e308 => "near f64 max"
    0.0 => "zero"
    value => "other"
```


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

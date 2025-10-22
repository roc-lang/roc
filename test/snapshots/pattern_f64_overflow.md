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
# EXPECTED
UNDEFINED VARIABLE - pattern_f64_overflow.md:1:7:1:8
F64 NOT ALLOWED IN PATTERN - :0:0:0:0
F64 NOT ALLOWED IN PATTERN - :0:0:0:0
F64 NOT ALLOWED IN PATTERN - :0:0:0:0
UNUSED VARIABLE - pattern_f64_overflow.md:6:5:6:10
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `x` in this scope.
Is there an `import` or `exposing` missing up-top?

**pattern_f64_overflow.md:1:7:1:8:**
```roc
match x {
```
      ^


**F64 NOT ALLOWED IN PATTERN**
This floating-point literal cannot be used in a pattern match: `1e100`

This number exceeds the precision range of Roc's `Dec` type and would require F64 representation. Floating-point numbers (F64) cannot be used in patterns because they don't have reliable equality comparison.

Consider one of these alternatives:
• Use a guard condition with a range check
• Use a smaller number that fits in Dec's precision
• Restructure your code to avoid pattern matching on this value

For example, instead of:
`1e100 => ...`
Use a guard:
`n if n > 1e99 => ...`

**F64 NOT ALLOWED IN PATTERN**
This floating-point literal cannot be used in a pattern match: `1e-40`

This number exceeds the precision range of Roc's `Dec` type and would require F64 representation. Floating-point numbers (F64) cannot be used in patterns because they don't have reliable equality comparison.

Consider one of these alternatives:
• Use a guard condition with a range check
• Use a smaller number that fits in Dec's precision
• Restructure your code to avoid pattern matching on this value

For example, instead of:
`1e100 => ...`
Use a guard:
`n if n > 1e99 => ...`

**F64 NOT ALLOWED IN PATTERN**
This floating-point literal cannot be used in a pattern match: `1.7976931348623157e308`

This number exceeds the precision range of Roc's `Dec` type and would require F64 representation. Floating-point numbers (F64) cannot be used in patterns because they don't have reliable equality comparison.

Consider one of these alternatives:
• Use a guard condition with a range check
• Use a smaller number that fits in Dec's precision
• Restructure your code to avoid pattern matching on this value

For example, instead of:
`1e100 => ...`
Use a guard:
`n if n > 1e99 => ...`

**UNUSED VARIABLE**
Variable `value` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_value` to suppress this warning.
The unused variable is declared here:
**pattern_f64_overflow.md:6:5:6:10:**
```roc
    value => "other"
```
    ^^^^^


# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
Float,OpFatArrow,StringStart,StringPart,StringEnd,
Float,OpFatArrow,StringStart,StringPart,StringEnd,
Float,OpFatArrow,StringStart,StringPart,StringEnd,
Float,OpFatArrow,StringStart,StringPart,StringEnd,
LowerIdent,OpFatArrow,StringStart,StringPart,StringEnd,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "x"))
	(branches
		(branch
			(p-frac (raw "1e100"))
			(e-string
				(e-string-part (raw "very large number"))))
		(branch
			(p-frac (raw "1e-40"))
			(e-string
				(e-string-part (raw "very small number"))))
		(branch
			(p-frac (raw "1.7976931348623157e308"))
			(e-string
				(e-string-part (raw "near f64 max"))))
		(branch
			(p-frac (raw "0.0"))
			(e-string
				(e-string-part (raw "zero"))))
		(branch
			(p-ident (raw "value"))
			(e-string
				(e-string-part (raw "other"))))))
~~~
# FORMATTED
~~~roc
match x {
	1e100 => "very large number"
	1e-40 => "very small number"
	1.7976931348623157e308 => "near f64 max"
	0.0 => "zero"
	value => "other"
}
~~~
# CANONICALIZE
~~~clojure
(e-match
	(match
		(cond
			(e-runtime-error (tag "ident_not_in_scope")))
		(branches
			(branch
				(patterns
					(pattern (degenerate false)
						(p-runtime-error (tag "f64_pattern_literal"))))
				(value
					(e-string
						(e-literal (string "very large number")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-runtime-error (tag "f64_pattern_literal"))))
				(value
					(e-string
						(e-literal (string "very small number")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-runtime-error (tag "f64_pattern_literal"))))
				(value
					(e-string
						(e-literal (string "near f64 max")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-small-dec)))
				(value
					(e-string
						(e-literal (string "zero")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-assign (ident "value"))))
				(value
					(e-string
						(e-literal (string "other"))))))))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~

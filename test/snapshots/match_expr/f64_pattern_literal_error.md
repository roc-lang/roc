# META
~~~ini
description=Match expression with f64 literal pattern (should error)
type=expr
~~~
# SOURCE
~~~roc
match x {
    3.14f64 => "pi"
    0.0f64 => "zero"
    value => "other"
}
~~~
# EXPECTED
UNDEFINED VARIABLE - f64_pattern_literal_error.md:1:7:1:8
DEPRECATED NUMBER SUFFIX - f64_pattern_literal_error.md:2:5:2:12
F64 NOT ALLOWED IN PATTERN - :0:0:0:0
DEPRECATED NUMBER SUFFIX - f64_pattern_literal_error.md:3:5:3:11
F64 NOT ALLOWED IN PATTERN - :0:0:0:0
UNUSED VARIABLE - f64_pattern_literal_error.md:4:5:4:10
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `x` in this scope.
Is there an `import` or `exposing` missing up-top?

**f64_pattern_literal_error.md:1:7:1:8:**
```roc
match x {
```
      ^


**DEPRECATED NUMBER SUFFIX**
This number literal uses a deprecated suffix syntax:

**f64_pattern_literal_error.md:2:5:2:12:**
```roc
    3.14f64 => "pi"
```
    ^^^^^^^

The `f64` suffix is no longer supported. Use `3.14.F64` instead.

**F64 NOT ALLOWED IN PATTERN**
This floating-point literal cannot be used in a pattern match: `3.14f64`

This number exceeds the precision range of Roc's `Dec` type and would require F64 representation. Floating-point numbers (F64) cannot be used in patterns because they don't have reliable equality comparison.

Consider one of these alternatives:
• Use a guard condition with a range check
• Use a smaller number that fits in Dec's precision
• Restructure your code to avoid pattern matching on this value

For example, instead of:
`1e100 => ...`
Use a guard:
`n if n > 1e99 => ...`

**DEPRECATED NUMBER SUFFIX**
This number literal uses a deprecated suffix syntax:

**f64_pattern_literal_error.md:3:5:3:11:**
```roc
    0.0f64 => "zero"
```
    ^^^^^^

The `f64` suffix is no longer supported. Use `0.0.F64` instead.

**F64 NOT ALLOWED IN PATTERN**
This floating-point literal cannot be used in a pattern match: `0.0f64`

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
**f64_pattern_literal_error.md:4:5:4:10:**
```roc
    value => "other"
```
    ^^^^^


# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
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
			(p-frac (raw "3.14f64"))
			(e-string
				(e-string-part (raw "pi"))))
		(branch
			(p-frac (raw "0.0f64"))
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
	3.14f64 => "pi"
	0.0f64 => "zero"
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
						(e-literal (string "pi")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-runtime-error (tag "f64_pattern_literal"))))
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
(expr (type "Str"))
~~~

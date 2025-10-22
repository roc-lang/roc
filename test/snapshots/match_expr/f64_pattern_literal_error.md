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
						(p-frac-f64 (value "3.14e0"))))
				(value
					(e-string
						(e-literal (string "pi")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-frac-f64 (value "0e0"))))
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

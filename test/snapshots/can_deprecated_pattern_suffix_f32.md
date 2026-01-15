# META
~~~ini
description=Test deprecated f32 suffix in match pattern (not allowed)
type=expr
~~~
# SOURCE
~~~roc
|x| match x {
    3.14f32 => "pi-ish"
    _ => "other"
}
~~~
# EXPECTED
DEPRECATED NUMBER SUFFIX - can_deprecated_pattern_suffix_f32.md:2:5:2:12
F64 NOT ALLOWED IN PATTERN - :0:0:0:0
# PROBLEMS
**DEPRECATED NUMBER SUFFIX**
This number literal uses a deprecated suffix syntax:

**can_deprecated_pattern_suffix_f32.md:2:5:2:12:**
```roc
    3.14f32 => "pi-ish"
```
    ^^^^^^^

The `f32` suffix is no longer supported. Use `3.14.F32` instead.

**F64 NOT ALLOWED IN PATTERN**
This floating-point literal cannot be used in a pattern match: `3.14f32`

This number exceeds the precision range of Roc's `Dec` type and would require F64 representation. Floating-point numbers (F64) cannot be used in patterns because they don't have reliable equality comparison.

Consider one of these alternatives:
• Use a guard condition with a range check
• Use a smaller number that fits in Dec's precision
• Restructure your code to avoid pattern matching on this value

For example, instead of:
`1e100 => ...`
Use a guard:
`n if n > 1e99 => ...`

# TOKENS
~~~zig
OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
Float,OpFatArrow,StringStart,StringPart,StringEnd,
Underscore,OpFatArrow,StringStart,StringPart,StringEnd,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-lambda
	(args
		(p-ident (raw "x")))
	(e-match
		(e-ident (raw "x"))
		(branches
			(branch
				(p-frac (raw "3.14f32"))
				(e-string
					(e-string-part (raw "pi-ish"))))
			(branch
				(p-underscore)
				(e-string
					(e-string-part (raw "other")))))))
~~~
# FORMATTED
~~~roc
|x| match x {
	3.14f32 => "pi-ish"
	_ => "other"
}
~~~
# CANONICALIZE
~~~clojure
(e-lambda
	(args
		(p-assign (ident "x")))
	(e-match
		(match
			(cond
				(e-lookup-local
					(p-assign (ident "x"))))
			(branches
				(branch
					(patterns
						(pattern (degenerate false)
							(p-runtime-error (tag "f64_pattern_literal"))))
					(value
						(e-string
							(e-literal (string "pi-ish")))))
				(branch
					(patterns
						(pattern (degenerate false)
							(p-underscore)))
					(value
						(e-string
							(e-literal (string "other")))))))))
~~~
# TYPES
~~~clojure
(expr (type "Error -> Str"))
~~~

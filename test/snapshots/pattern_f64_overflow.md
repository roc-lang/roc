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
NIL
# PROBLEMS
NIL
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
(expr (type "Str"))
~~~

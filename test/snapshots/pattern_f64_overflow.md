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
INVALID NUMBER - pattern_f64_overflow.md:2:5:2:10
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Invalid Number")
		(region (start 2 5) (end 2 10))
		(headline
			(reflow "This number literal does not fit in the inferred type."))
		(document
			(source-region (file "pattern_f64_overflow.md") (start 2 5) (end 2 10) (annotation error) (line-text "    1e100 => \"very large number\""))
			(line-break)
			(reflow "The inferred type is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Dec")
			(annotation-end))))
~~~
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
(e-runtime-error (tag "erroneous_value_expr"))
~~~
# TYPES
~~~clojure
(expr (type "Str"))
~~~

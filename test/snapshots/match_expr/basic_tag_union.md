# META
~~~ini
description=Basic tag union match with simple patterns
type=expr
~~~
# SOURCE
~~~roc
match color {
	Red => 1
	Blue => 2
	Green => "3"
}
~~~
# EXPECTED
TYPE MISMATCH - basic_tag_union.md:4:11:4:14
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 4 11) (end 4 14))
		(headline
			(reflow "This string literal is being used where a non-string type is needed."))
		(document
			(source-region (file "basic_tag_union.md") (start 4 11) (end 4 14) (annotation error) (line-text "\tGreen => \"3\""))
			(line-break)
			(reflow "The type was determined to be:")
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
UpperIdent,OpFatArrow,Int,
UpperIdent,OpFatArrow,Int,
UpperIdent,OpFatArrow,StringStart,StringPart,StringEnd,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "color"))
	(branches
		(branch
			(p-tag (raw "Red"))
			(e-int (raw "1")))
		(branch
			(p-tag (raw "Blue"))
			(e-int (raw "2")))
		(branch
			(p-tag (raw "Green"))
			(e-string
				(e-string-part (raw "3"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
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
						(p-applied-tag)))
				(value
					(e-num (value "1"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-applied-tag)))
				(value
					(e-num (value "2"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-applied-tag)))
				(value
					(e-runtime-error (tag "erroneous_value_expr")))))))
~~~
# TYPES
~~~clojure
(expr (type "Dec"))
~~~

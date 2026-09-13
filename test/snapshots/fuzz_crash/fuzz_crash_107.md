# META
~~~ini
description=parser formatter stability: multiline string record field separator
type=file
~~~
# SOURCE
~~~roc
e={{e:\\
}.U{}}
~~~
# EXPECTED
NOT IMPLEMENTED - fuzz_crash_107.md:1:4:2:4
# PROBLEMS
~~~clojure
(reports
	(report
		(severity fatal)
		(title "Not Implemented")
		(region (start 1 4) (end 2 4))
		(headline
			(reflow "This feature is not yet implemented: ")
			(annotation-start emphasis)
			(text "single-field record builder (minimum 2 fields required)")
			(annotation-end)
			(reflow "."))
		(document
			(source-region (file "fuzz_crash_107.md") (start 1 4) (end 2 4) (annotation error) (line-text "e={{e:\\\\\n}.U{}}"))
			(line-break)
			(reflow "This error doesn't have a proper diagnostic report yet. Let us know if you want to help improve Roc's error messages!")
			(line-break))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,OpenCurly,OpenCurly,LowerIdent,OpColon,MultilineStringStart,StringPart,
CloseCurly,NoSpaceDotUpperIdent,OpenCurly,CloseCurly,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "e"))
			(e-block
				(statements
					(e-record-builder
						(mapper (e-tag (raw "U")))
						(field (field "e")
							(e-multiline-string
								(e-string-part (raw "")))))
					(e-record))))))
~~~
# FORMATTED
~~~roc
e = {
	{
		e: \\
		,
	}.U
	{}
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "e"))
		(e-block
			(s-expr
				(e-runtime-error (tag "not_implemented")))
			(e-empty_record))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "{}")))
	(expressions
		(expr (type "{}"))))
~~~

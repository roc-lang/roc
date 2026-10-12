# META
~~~ini
description=A numeral interpolated into a string whose type is only decided by literal defaulting is reported with its literal type, never the Dec default
type=snippet
~~~
# SOURCE
~~~roc
ma = |_| {
	var $er = 123
	line!("Ag ${$er}")
}
~~~
# EXPECTED
NAME NOT IN SCOPE - interpolation_defaulted_numeral_part_type.md:3:2:3:7
TYPE MISMATCH - interpolation_defaulted_numeral_part_type.md:3:14:3:17
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 3 2) (end 3 7))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "line!")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "interpolation_defaulted_numeral_part_type.md") (start 3 2) (end 3 7) (annotation error) (line-text "\tline!(\"Ag ${$er}\")"))))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 3 14) (end 3 17))
		(headline
			(reflow "This expression is used in an unexpected way."))
		(document
			(source-region (file "interpolation_defaulted_numeral_part_type.md") (start 3 14) (end 3 17) (annotation error) (line-text "\tline!(\"Ag ${$er}\")"))
			(line-break)
			(reflow "It has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But you are trying to use it as:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Str")
			(annotation-end))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
KwVar,LowerIdent,OpAssign,Int,
LowerIdent,NoSpaceOpenRound,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "ma"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-var (name "$er")
							(e-int (raw "123")))
						(e-apply
							(e-ident (raw "line!"))
							(e-string
								(e-string-part (raw "Ag "))
								(e-ident (raw "$er"))
								(e-string-part (raw ""))))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "ma"))
		(e-lambda
			(args
				(p-underscore))
			(e-block
				(s-var
					(p-var-assign (ident "$er"))
					(e-num (value "123")))
				(e-runtime-error (tag "erroneous_value_expr"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_arg -> _ret")))
	(expressions
		(expr (type "_arg -> _ret"))))
~~~

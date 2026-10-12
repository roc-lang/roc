# META
~~~ini
description=A custom from_interpolation decides the item type, so the value that does not match it is the one reported, wherever it appears
type=file
~~~
# SOURCE
~~~roc
Html := [Text(Str)].{
	from_interpolation : List(Str) -> Try((List(Html) -> Html), [InvalidInterpolation(Str)])
	from_interpolation = |_segments| Ok(|_items| Html.Text(""))
}

main! = |_args| {
	title = Html.Text("t")
	label : Str
	label = "x"
	page : Html
	page = "${label}: ${title}"
	_ = page
	Ok({})
}
~~~
# EXPECTED
TYPE MISMATCH - interpolation_custom_target_decides_parts.md:11:12:11:17
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 11 12) (end 11 17))
		(headline
			(reflow "This expression is used in an unexpected way."))
		(document
			(source-region (file "interpolation_custom_target_decides_parts.md") (start 11 12) (end 11 17) (annotation error) (line-text "\tpage = \"${label}: ${title}\""))
			(line-break)
			(reflow "It has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Str")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But you are trying to use it as:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Html")
			(annotation-end))))
~~~
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,OpArrow,UpperIdent,CloseRound,Comma,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,NamedUnderscore,OpBar,UpperIdent,NoSpaceOpenRound,OpBar,NamedUnderscore,OpBar,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,CloseRound,
CloseCurly,
LowerIdent,OpAssign,OpBar,NamedUnderscore,OpBar,OpenCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,
Underscore,OpAssign,LowerIdent,
UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Html")
				(args))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "Text"))
						(ty (name "Str")))))
			(associated
				(s-type-anno (name "from_interpolation")
					(ty-fn
						(ty-apply
							(ty (name "List"))
							(ty (name "Str")))
						(ty-apply
							(ty (name "Try"))
							(ty-fn
								(ty-apply
									(ty (name "List"))
									(ty (name "Html")))
								(ty (name "Html")))
							(ty-tag-union
								(tags
									(ty-apply
										(ty (name "InvalidInterpolation"))
										(ty (name "Str"))))))))
				(s-decl
					(p-ident (raw "from_interpolation"))
					(e-lambda
						(args
							(p-ident (raw "_segments")))
						(e-apply
							(e-tag (raw "Ok"))
							(e-lambda
								(args
									(p-ident (raw "_items")))
								(e-apply
									(e-tag (raw "Html.Text"))
									(e-string
										(e-string-part (raw ""))))))))))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-ident (raw "_args")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "title"))
							(e-apply
								(e-tag (raw "Html.Text"))
								(e-string
									(e-string-part (raw "t")))))
						(s-type-anno (name "label")
							(ty (name "Str")))
						(s-decl
							(p-ident (raw "label"))
							(e-string
								(e-string-part (raw "x"))))
						(s-type-anno (name "page")
							(ty (name "Html")))
						(s-decl
							(p-ident (raw "page"))
							(e-string
								(e-string-part (raw ""))
								(e-ident (raw "label"))
								(e-string-part (raw ": "))
								(e-ident (raw "title"))
								(e-string-part (raw ""))))
						(s-decl
							(p-underscore)
							(e-ident (raw "page")))
						(e-apply
							(e-tag (raw "Ok"))
							(e-record))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "echo!"))
		(e-hosted-lambda (symbol "echo!")
			(args
				(p-assign (ident "_echo_arg"))))
		(annotation
			(ty-fn (effectful true)
				(ty-lookup (name "Str") (builtin))
				(ty-record))))
	(d-let
		(p-assign (ident "interpolation_custom_target_decides_parts.Html.from_interpolation"))
		(e-lambda
			(args
				(p-assign (ident "_segments")))
			(e-tag (name "Ok")
				(args
					(e-lambda
						(args
							(p-assign (ident "_items")))
						(e-nominal (nominal "Html")
							(e-tag (name "Text")
								(args
									(e-string
										(e-literal (string ""))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "List") (builtin)
					(ty-lookup (name "Str") (builtin)))
				(ty-apply (name "Try") (builtin)
					(ty-parens
						(ty-fn (effectful false)
							(ty-apply (name "List") (builtin)
								(ty-lookup (name "Html") (local)))
							(ty-lookup (name "Html") (local))))
					(ty-tag-union
						(ty-tag-name (name "InvalidInterpolation")
							(ty-lookup (name "Str") (builtin))))))))
	(d-let
		(p-assign (ident "main!"))
		(e-lambda
			(args
				(p-assign (ident "_args")))
			(e-block
				(s-let
					(p-assign (ident "title"))
					(e-nominal (nominal "Html")
						(e-tag (name "Text")
							(args
								(e-string
									(e-literal (string "t")))))))
				(s-let
					(p-assign (ident "label"))
					(e-string
						(e-literal (string "x"))))
				(s-let
					(p-assign (ident "page"))
					(e-block
						(s-let
							(p-assign (ident "#interp_0"))
							(e-lookup-local
								(p-assign (ident "label"))))
						(s-let
							(p-assign (ident "#interp_1"))
							(e-lookup-local
								(p-assign (ident "title"))))
						(e-runtime-error (tag "erroneous_value_expr")
							(e-literal (string "")))))
				(s-let
					(p-underscore)
					(e-lookup-local
						(p-assign (ident "page"))))
				(e-tag (name "Ok")
					(args
						(e-empty_record))))))
	(s-nominal-decl
		(ty-header (name "Html"))
		(ty-tag-union
			(ty-tag-name (name "Text")
				(ty-lookup (name "Str") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str => {}"))
		(patt (type "List(Str) -> Try(List(Error) -> Html, [InvalidInterpolation(Str)])"))
		(patt (type "_arg -> [Ok({})]")))
	(type_decls
		(nominal (type "Html")
			(ty-header (name "Html"))))
	(expressions
		(expr (type "Str => {}"))
		(expr (type "List(Str) -> Try(List(Error) -> Html, [InvalidInterpolation(Str)])"))
		(expr (type "_arg -> [Ok({})]"))))
~~~

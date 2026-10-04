# META
~~~ini
description=An interpolation cannot target Try, since Try has no from_interpolation
type=snippet
~~~
# SOURCE
~~~roc
Url := [Url(Str)].{
    from_interpolation : List(Str) -> Try((List(Str) -> Url), [InvalidInterpolation(Str)])
    from_interpolation = |segments| Str.from_interpolation(segments).map_ok(|assemble| |values| Url.Url(assemble(values)))
}

main = {
    domain = "example"
    url : Try(Url, [InvalidInterpolation(Str)])
    url = "https://${domain}.com"
    url
}
~~~
# EXPECTED
TYPE MISMATCH - interpolation_try_target.md:9:11:9:34
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 9 11) (end 9 34))
		(headline
			(reflow "This string literal is being used where a non-string type is needed."))
		(document
			(source-region (file "interpolation_try_target.md") (start 9 11) (end 9 34) (annotation error) (line-text "    url = \"https://${domain}.com\""))
			(line-break)
			(reflow "The type was determined to be:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Try(Url, [InvalidInterpolation(Str)])")
			(annotation-end))))
~~~
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,OpArrow,UpperIdent,CloseRound,Comma,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceDotLowerIdent,NoSpaceOpenRound,OpBar,LowerIdent,OpBar,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,CloseRound,
CloseCurly,
LowerIdent,OpAssign,OpenCurly,
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,CloseRound,
LowerIdent,OpAssign,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,
LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Url")
				(args))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "Url"))
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
									(ty (name "Str")))
								(ty (name "Url")))
							(ty-tag-union
								(tags
									(ty-apply
										(ty (name "InvalidInterpolation"))
										(ty (name "Str"))))))))
				(s-decl
					(p-ident (raw "from_interpolation"))
					(e-lambda
						(args
							(p-ident (raw "segments")))
						(e-method-call (method ".map_ok")
							(receiver
								(e-apply
									(e-ident (raw "Str.from_interpolation"))
									(e-ident (raw "segments"))))
							(args
								(e-lambda
									(args
										(p-ident (raw "assemble")))
									(e-lambda
										(args
											(p-ident (raw "values")))
										(e-apply
											(e-tag (raw "Url.Url"))
											(e-apply
												(e-ident (raw "assemble"))
												(e-ident (raw "values"))))))))))))
		(s-decl
			(p-ident (raw "main"))
			(e-block
				(statements
					(s-decl
						(p-ident (raw "domain"))
						(e-string
							(e-string-part (raw "example"))))
					(s-type-anno (name "url")
						(ty-apply
							(ty (name "Try"))
							(ty (name "Url"))
							(ty-tag-union
								(tags
									(ty-apply
										(ty (name "InvalidInterpolation"))
										(ty (name "Str")))))))
					(s-decl
						(p-ident (raw "url"))
						(e-string
							(e-string-part (raw "https://"))
							(e-ident (raw "domain"))
							(e-string-part (raw ".com"))))
					(e-ident (raw "url")))))))
~~~
# FORMATTED
~~~roc
Url := [Url(Str)].{
	from_interpolation : List(Str) -> Try((List(Str) -> Url), [InvalidInterpolation(Str)])
	from_interpolation = |segments| Str.from_interpolation(segments).map_ok(|assemble| |values| Url.Url(assemble(values)))
}

main = {
	domain = "example"
	url : Try(Url, [InvalidInterpolation(Str)])
	url = "https://${domain}.com"
	url
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "interpolation_try_target.Url.from_interpolation"))
		(e-lambda
			(args
				(p-assign (ident "segments")))
			(e-dispatch-call (method "map_ok") (constraint-fn-var 319)
				(receiver
					(e-call (constraint-fn-var 316)
						(e-lookup-external
							(builtin))
						(e-lookup-local
							(p-assign (ident "segments")))))
				(args
					(e-lambda
						(args
							(p-assign (ident "assemble")))
						(e-closure
							(captures
								(capture (ident "assemble")))
							(e-lambda
								(args
									(p-assign (ident "values")))
								(e-nominal (nominal "Url")
									(e-tag (name "Url")
										(args
											(e-call (constraint-fn-var 347)
												(e-lookup-local
													(p-assign (ident "assemble")))
												(e-lookup-local
													(p-assign (ident "values")))))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "List") (builtin)
					(ty-lookup (name "Str") (builtin)))
				(ty-apply (name "Try") (builtin)
					(ty-parens
						(ty-fn (effectful false)
							(ty-apply (name "List") (builtin)
								(ty-lookup (name "Str") (builtin)))
							(ty-lookup (name "Url") (local))))
					(ty-tag-union
						(ty-tag-name (name "InvalidInterpolation")
							(ty-lookup (name "Str") (builtin))))))))
	(d-let
		(p-assign (ident "main"))
		(e-block
			(s-let
				(p-assign (ident "domain"))
				(e-string
					(e-literal (string "example"))))
			(s-let
				(p-assign (ident "url"))
				(e-block
					(s-let
						(p-assign (ident "#interp_0"))
						(e-lookup-local
							(p-assign (ident "domain"))))
					(e-runtime-error (tag "erroneous_value_expr"))))
			(e-lookup-local
				(p-assign (ident "url")))))
	(s-nominal-decl
		(ty-header (name "Url"))
		(ty-tag-union
			(ty-tag-name (name "Url")
				(ty-lookup (name "Str") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "List(Str) -> Try(List(Str) -> Url, [InvalidInterpolation(Str)])"))
		(patt (type "Try(Url, [InvalidInterpolation(Str)])")))
	(type_decls
		(nominal (type "Url")
			(ty-header (name "Url"))))
	(expressions
		(expr (type "List(Str) -> Try(List(Str) -> Url, [InvalidInterpolation(Str)])"))
		(expr (type "Try(Url, [InvalidInterpolation(Str)])"))))
~~~

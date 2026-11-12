# META
~~~ini
description=An exhaustive format callback rejects a payload parser with a different error
type=snippet
~~~
# SOURCE
~~~roc
Format := {}.{
	parse_tag_union : Format, Encoding.ParseTagUnionSpec(a, Format, Str, [Syntax(Str)]), Str -> Try({ value : a, rest : Str }, [Syntax(Str)])
	parse_tag_union = |encoding, spec, state| {
		result = Encoding.ParseTagUnionSpec.parse(spec, {
			tag: "Item",
			encoding,
			state,
			start_payloads: |s, _| Ok(s),
			next_payload: |s, _, _| Ok(s),
			finish_payloads: |s, _| Ok(s),
			missing: Syntax("unknown tag"),
		})
		match result {
			Ok(value) => Ok(value)
			Err(Syntax(message)) => Err(Syntax("caught ${message}"))
		}
	}
}

Payload := {}.{
	parser_for : Format -> (Str -> Try({ value : Payload, rest : Str }, [PayloadError(Str)]))
	parser_for = |_| |_| Err(PayloadError("child"))
}

expect {
	Shape : [Item(Payload)]
	parse = Shape.parser_for(Format.{})
	match parse("") {
		Err(PayloadError("child")) => True
		_ => False
	}
}
~~~
# EXPECTED
PARSER ERROR TYPE MISMATCH - parser_format_owned_payload_error_rejected.md:27:10:27:37
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Parser Error Type Mismatch")
		(region (start 27 10) (end 27 37))
		(headline
			(reflow "Every operation in a derived parser must use the same format-owned error type."))
		(document
			(source-region (file "parser_format_owned_payload_error_rejected.md") (start 27 10) (end 27 37) (annotation error) (line-text "\tparse = Shape.parser_for(Format.{})"))
			(line-break)
			(reflow "One operation uses the error type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[Syntax(Str)]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But this parser requires:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[PayloadError(Str)]")
			(annotation-end)
			(line-break)
			(reflow "Use the format's error type in each parser operation, translating custom domain errors explicitly."))))
~~~
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenCurly,CloseCurly,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,Comma,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,Comma,UpperIdent,Comma,UpperIdent,Comma,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,CloseRound,Comma,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,OpenCurly,LowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,Comma,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,Comma,OpenCurly,
LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,
LowerIdent,Comma,
LowerIdent,Comma,
LowerIdent,OpColon,OpBar,LowerIdent,Comma,Underscore,OpBar,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,
LowerIdent,OpColon,OpBar,LowerIdent,Comma,Underscore,Comma,Underscore,OpBar,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,
LowerIdent,OpColon,OpBar,LowerIdent,Comma,Underscore,OpBar,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,Comma,
CloseCurly,CloseRound,
KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,OpFatArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,CloseRound,CloseRound,
CloseCurly,
CloseCurly,
CloseCurly,
UpperIdent,OpColonEqual,OpenCurly,CloseCurly,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,OpenRound,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,Comma,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,CloseRound,CloseRound,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpBar,Underscore,OpBar,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,CloseRound,
CloseCurly,
KwExpect,OpenCurly,
UpperIdent,OpColon,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,UpperIdent,Dot,OpenCurly,CloseCurly,CloseRound,
KwMatch,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,OpenCurly,
UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,CloseRound,OpFatArrow,UpperIdent,
Underscore,OpFatArrow,UpperIdent,
CloseCurly,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Format")
				(args))
			(ty-record)
			(associated
				(s-type-anno (name "parse_tag_union")
					(ty-fn
						(ty (name "Format"))
						(ty-apply
							(ty (name "Encoding.ParseTagUnionSpec"))
							(ty-var (raw "a"))
							(ty (name "Format"))
							(ty (name "Str"))
							(ty-tag-union
								(tags
									(ty-apply
										(ty (name "Syntax"))
										(ty (name "Str"))))))
						(ty (name "Str"))
						(ty-apply
							(ty (name "Try"))
							(ty-record
								(anno-record-field (name "value")
									(ty-var (raw "a")))
								(anno-record-field (name "rest")
									(ty (name "Str"))))
							(ty-tag-union
								(tags
									(ty-apply
										(ty (name "Syntax"))
										(ty (name "Str"))))))))
				(s-decl
					(p-ident (raw "parse_tag_union"))
					(e-lambda
						(args
							(p-ident (raw "encoding"))
							(p-ident (raw "spec"))
							(p-ident (raw "state")))
						(e-block
							(statements
								(s-decl
									(p-ident (raw "result"))
									(e-apply
										(e-ident (raw "Encoding.ParseTagUnionSpec.parse"))
										(e-ident (raw "spec"))
										(e-record
											(field (field "tag")
												(e-string
													(e-string-part (raw "Item"))))
											(field (field "encoding"))
											(field (field "state"))
											(field (field "start_payloads")
												(e-lambda
													(args
														(p-ident (raw "s"))
														(p-underscore))
													(e-apply
														(e-tag (raw "Ok"))
														(e-ident (raw "s")))))
											(field (field "next_payload")
												(e-lambda
													(args
														(p-ident (raw "s"))
														(p-underscore)
														(p-underscore))
													(e-apply
														(e-tag (raw "Ok"))
														(e-ident (raw "s")))))
											(field (field "finish_payloads")
												(e-lambda
													(args
														(p-ident (raw "s"))
														(p-underscore))
													(e-apply
														(e-tag (raw "Ok"))
														(e-ident (raw "s")))))
											(field (field "missing")
												(e-apply
													(e-tag (raw "Syntax"))
													(e-string
														(e-string-part (raw "unknown tag"))))))))
								(e-match
									(e-ident (raw "result"))
									(branches
										(branch
											(p-tag (raw "Ok")
												(p-ident (raw "value")))
											(e-apply
												(e-tag (raw "Ok"))
												(e-ident (raw "value"))))
										(branch
											(p-tag (raw "Err")
												(p-tag (raw "Syntax")
													(p-ident (raw "message"))))
											(e-apply
												(e-tag (raw "Err"))
												(e-apply
													(e-tag (raw "Syntax"))
													(e-string
														(e-string-part (raw "caught "))
														(e-ident (raw "message"))
														(e-string-part (raw ""))))))))))))))
		(s-type-decl
			(header (name "Payload")
				(args))
			(ty-record)
			(associated
				(s-type-anno (name "parser_for")
					(ty-fn
						(ty (name "Format"))
						(ty-fn
							(ty (name "Str"))
							(ty-apply
								(ty (name "Try"))
								(ty-record
									(anno-record-field (name "value")
										(ty (name "Payload")))
									(anno-record-field (name "rest")
										(ty (name "Str"))))
								(ty-tag-union
									(tags
										(ty-apply
											(ty (name "PayloadError"))
											(ty (name "Str")))))))))
				(s-decl
					(p-ident (raw "parser_for"))
					(e-lambda
						(args
							(p-underscore))
						(e-lambda
							(args
								(p-underscore))
							(e-apply
								(e-tag (raw "Err"))
								(e-apply
									(e-tag (raw "PayloadError"))
									(e-string
										(e-string-part (raw "child"))))))))))
		(s-expect
			(e-block
				(statements
					(s-type-decl
						(header (name "Shape")
							(args))
						(ty-tag-union
							(tags
								(ty-apply
									(ty (name "Item"))
									(ty (name "Payload"))))))
					(s-decl
						(p-ident (raw "parse"))
						(e-apply
							(e-ident (raw "Shape.parser_for"))
							(e-nominal-record
								(mapper (e-tag (raw "Format")))
								(backing (e-record)))))
					(e-match
						(e-apply
							(e-ident (raw "parse"))
							(e-string
								(e-string-part (raw ""))))
						(branches
							(branch
								(p-tag (raw "Err")
									(p-tag (raw "PayloadError")
										(p-string (raw """)
											(p-string-text (raw "child")))))
								(e-tag (raw "True")))
							(branch
								(p-underscore)
								(e-tag (raw "False"))))))))))
~~~
# FORMATTED
~~~roc
Format := {}.{
	parse_tag_union : Format, Encoding.ParseTagUnionSpec(a, Format, Str, [Syntax(Str)]), Str -> Try({ value : a, rest : Str }, [Syntax(Str)])
	parse_tag_union = |encoding, spec, state| {
		result = Encoding.ParseTagUnionSpec.parse(
			spec,
			{
				tag: "Item",
				encoding,
				state,
				start_payloads: |s, _| Ok(s),
				next_payload: |s, _, _| Ok(s),
				finish_payloads: |s, _| Ok(s),
				missing: Syntax("unknown tag"),
			},
		)
		match result {
			Ok(value) => Ok(value)
			Err(Syntax(message)) => Err(Syntax("caught ${message}"))
		}
	}
}

Payload := {}.{
	parser_for : Format -> (Str -> Try({ value : Payload, rest : Str }, [PayloadError(Str)]))
	parser_for = |_| |_| Err(PayloadError("child"))
}

expect {
	Shape : [Item(Payload)]
	parse = Shape.parser_for(Format.{})
	match parse("") {
		Err(PayloadError("child")) => True
		_ => False
	}
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "parser_format_owned_payload_error_rejected.Format.parse_tag_union"))
		(e-lambda
			(args
				(p-assign (ident "encoding"))
				(p-assign (ident "spec"))
				(p-assign (ident "state")))
			(e-block
				(s-let
					(p-assign (ident "result"))
					(e-call (constraint-fn-var 548)
						(e-lookup-external
							(builtin))
						(e-lookup-local
							(p-assign (ident "spec")))
						(e-record
							(fields
								(field (name "tag")
									(e-string
										(e-literal (string "Item"))))
								(field (name "encoding")
									(e-lookup-local
										(p-assign (ident "encoding"))))
								(field (name "state")
									(e-lookup-local
										(p-assign (ident "state"))))
								(field (name "start_payloads")
									(e-lambda
										(args
											(p-assign (ident "s"))
											(p-underscore))
										(e-tag (name "Ok")
											(args
												(e-lookup-local
													(p-assign (ident "s")))))))
								(field (name "next_payload")
									(e-lambda
										(args
											(p-assign (ident "s"))
											(p-underscore)
											(p-underscore))
										(e-tag (name "Ok")
											(args
												(e-lookup-local
													(p-assign (ident "s")))))))
								(field (name "finish_payloads")
									(e-lambda
										(args
											(p-assign (ident "s"))
											(p-underscore))
										(e-tag (name "Ok")
											(args
												(e-lookup-local
													(p-assign (ident "s")))))))
								(field (name "missing")
									(e-tag (name "Syntax")
										(args
											(e-string
												(e-literal (string "unknown tag"))))))))))
				(e-match
					(match
						(cond
							(e-lookup-local
								(p-assign (ident "result"))))
						(branches
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag)))
								(value
									(e-tag (name "Ok")
										(args
											(e-lookup-local
												(p-assign (ident "value")))))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag)))
								(value
									(e-tag (name "Err")
										(args
											(e-tag (name "Syntax")
												(args
													(e-block
														(s-let
															(p-assign (ident "#interp_0"))
															(e-lookup-local
																(p-assign (ident "message"))))
														(e-interpolation (constraint-fn-var 582) (dispatcher-var 83)
															(first
																(e-literal (string "caught ")))
															(parts
																(e-lookup-local
																	(p-assign (ident "#interp_0")))
																(e-literal (string ""))))))))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Format") (local))
				(ty-apply (name "Encoding.ParseTagUnionSpec") (builtin)
					(ty-rigid-var (name "a"))
					(ty-lookup (name "Format") (local))
					(ty-lookup (name "Str") (builtin))
					(ty-tag-union
						(ty-tag-name (name "Syntax")
							(ty-lookup (name "Str") (builtin)))))
				(ty-lookup (name "Str") (builtin))
				(ty-apply (name "Try") (builtin)
					(ty-record
						(field (field "value")
							(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
						(field (field "rest")
							(ty-lookup (name "Str") (builtin))))
					(ty-tag-union
						(ty-tag-name (name "Syntax")
							(ty-lookup (name "Str") (builtin))))))))
	(d-let
		(p-assign (ident "parser_format_owned_payload_error_rejected.Payload.parser_for"))
		(e-lambda
			(args
				(p-underscore))
			(e-lambda
				(args
					(p-underscore))
				(e-tag (name "Err")
					(args
						(e-tag (name "PayloadError")
							(args
								(e-string
									(e-literal (string "child")))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Format") (local))
				(ty-parens
					(ty-fn (effectful false)
						(ty-lookup (name "Str") (builtin))
						(ty-apply (name "Try") (builtin)
							(ty-record
								(field (field "value")
									(ty-lookup (name "Payload") (local)))
								(field (field "rest")
									(ty-lookup (name "Str") (builtin))))
							(ty-tag-union
								(ty-tag-name (name "PayloadError")
									(ty-lookup (name "Str") (builtin))))))))))
	(s-nominal-decl
		(ty-header (name "Format"))
		(ty-record))
	(s-nominal-decl
		(ty-header (name "Payload"))
		(ty-record))
	(s-expect
		(e-block
			(s-alias-decl
				(ty-header (name "Shape"))
				(ty-tag-union
					(ty-tag-name (name "Item")
						(ty-lookup (name "Payload") (local)))))
			(s-let
				(p-assign (ident "parse"))
				(e-runtime-error (tag "erroneous_value_expr")
					(e-nominal (nominal "Format")
						(e-empty_record))))
			(e-match
				(match
					(cond
						(e-call (constraint-fn-var 642)
							(e-lookup-local
								(p-assign (ident "parse")))
							(e-string
								(e-literal (string "")))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-tag (name "True"))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-underscore)))
							(value
								(e-tag (name "False")))))))))
	(s-alias-decl
		(ty-header (name "Shape"))
		(ty-tag-union
			(ty-tag-name (name "Item")
				(ty-lookup (name "Payload") (local))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Format, Encoding.ParseTagUnionSpec(a, Format, Str, [Syntax(Str)]), Str -> Try({ rest: Str, value: a }, [Syntax(Str)])"))
		(patt (type "Format -> (Str -> Try({ rest: Str, value: Payload }, [PayloadError(Str)]))")))
	(type_decls
		(nominal (type "Format")
			(ty-header (name "Format")))
		(nominal (type "Payload")
			(ty-header (name "Payload")))
		(alias (type "Shape")
			(ty-header (name "Shape"))))
	(expressions
		(expr (type "Format, Encoding.ParseTagUnionSpec(a, Format, Str, [Syntax(Str)]), Str -> Try({ rest: Str, value: a }, [Syntax(Str)])"))
		(expr (type "Format -> (Str -> Try({ rest: Str, value: Payload }, [PayloadError(Str)]))"))))
~~~

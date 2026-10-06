# META
~~~ini
description=Explain forwarding a closed error row into a larger error union without blaming a host
type=snippet
~~~
# SOURCE
~~~roc
forward : Try({}, [StdoutErr(Str)]) -> Try({}, [Exit(I32), StdoutErr(Str)])
forward = |result| {
    result?
    Ok({})
}

# Reconstructing the variant at the return type is accepted.
reconstruct : Try({}, [StdoutErr(Str)]) -> Try({}, [Exit(I32), StdoutErr(Str)])
reconstruct = |result| match result {
    Ok(value) => Ok(value)
    Err(StdoutErr(message)) => Err(StdoutErr(message))
}

# A disjoint error row should retain the ordinary mismatch hint.
disjoint : Try({}, [Other]) -> Try({}, [Exit(I32)])
disjoint = |result| {
    result?
    Ok({})
}

# Matching tag names do not imply matching payload types.
payload_mismatch : Try({}, [Problem(Str)]) -> Try({}, [Extra, Problem(I32)])
payload_mismatch = |result| {
    result?
    Ok({})
}
~~~
# EXPECTED
TYPE MISMATCH - try_suffix_closed_error_row.md:3:5:3:12
TYPE MISMATCH - try_suffix_closed_error_row.md:17:5:17:12
TYPE MISMATCH - try_suffix_closed_error_row.md:24:5:24:12
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 3 5) (end 3 12))
		(headline
			(reflow "This")
			(reflow " ")
			(annotated code "?")
			(reflow " ")
			(reflow "may return early with a type that doesn't match the function body."))
		(document
			(source-region (file "try_suffix_closed_error_row.md") (start 3 5) (end 3 12) (annotation error) (line-text "    result?"))
			(line-break)
			(reflow "If this")
			(reflow " ")
			(annotated code "Try")
			(reflow " ")
			(reflow "is an")
			(reflow " ")
			(annotated code "Err")
			(reflow ",")
			(reflow " ")
			(reflow "then the")
			(reflow " ")
			(annotated code "?")
			(reflow " ")
			(reflow "after it immediately returns an")
			(reflow " ")
			(annotated code "Err")
			(reflow " ")
			(reflow "whose payload has this type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[StdoutErr(Str)]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "Returning an")
			(reflow " ")
			(annotated code "Err")
			(reflow " ")
			(reflow "with that type only works if the function itself returns a")
			(reflow " ")
			(annotated code "Try")
			(reflow " ")
			(reflow "with a compatible error type, but this function's return type is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Try({}, [Exit(I32), StdoutErr(Str)])")
			(annotation-end)
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "This error payload is a closed tag union. It cannot gain the additional variants in the function's error type, even when all its variants are listed there. The two unions can have different runtime representations.")
			(line-break)
			(line-break)
			(reflow "Use an explicit")
			(reflow " ")
			(annotated code "match")
			(reflow " ")
			(reflow "to reconstruct each error variant in the return type, or wrap the original error in a tag of that type. Reconstructed payloads must also match the expected payload types. Forwarding the unchanged error payload does not convert it.")))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 17 5) (end 17 12))
		(headline
			(reflow "This")
			(reflow " ")
			(annotated code "?")
			(reflow " ")
			(reflow "may return early with a type that doesn't match the function body."))
		(document
			(source-region (file "try_suffix_closed_error_row.md") (start 17 5) (end 17 12) (annotation error) (line-text "    result?"))
			(line-break)
			(reflow "If this")
			(reflow " ")
			(annotated code "Try")
			(reflow " ")
			(reflow "is an")
			(reflow " ")
			(annotated code "Err")
			(reflow ",")
			(reflow " ")
			(reflow "then the")
			(reflow " ")
			(annotated code "?")
			(reflow " ")
			(reflow "after it immediately returns an")
			(reflow " ")
			(annotated code "Err")
			(reflow " ")
			(reflow "whose payload has this type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[Other]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "Returning an")
			(reflow " ")
			(annotated code "Err")
			(reflow " ")
			(reflow "with that type only works if the function itself returns a")
			(reflow " ")
			(annotated code "Try")
			(reflow " ")
			(reflow "with a compatible error type, but this function's return type is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Try({}, [Exit(I32)])")
			(annotation-end)
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "The error types from all")
			(reflow " ")
			(annotated code "?")
			(reflow " ")
			(reflow "operators and the function body must be compatible, since any of them could be the actual return value.")))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 24 5) (end 24 12))
		(headline
			(reflow "This")
			(reflow " ")
			(annotated code "?")
			(reflow " ")
			(reflow "may return early with a type that doesn't match the function body."))
		(document
			(source-region (file "try_suffix_closed_error_row.md") (start 24 5) (end 24 12) (annotation error) (line-text "    result?"))
			(line-break)
			(reflow "If this")
			(reflow " ")
			(annotated code "Try")
			(reflow " ")
			(reflow "is an")
			(reflow " ")
			(annotated code "Err")
			(reflow ",")
			(reflow " ")
			(reflow "then the")
			(reflow " ")
			(annotated code "?")
			(reflow " ")
			(reflow "after it immediately returns an")
			(reflow " ")
			(annotated code "Err")
			(reflow " ")
			(reflow "whose payload has this type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[Problem(Str)]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "Returning an")
			(reflow " ")
			(annotated code "Err")
			(reflow " ")
			(reflow "with that type only works if the function itself returns a")
			(reflow " ")
			(annotated code "Try")
			(reflow " ")
			(reflow "with a compatible error type, but this function's return type is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Try({}, [Extra, Problem(I32)])")
			(annotation-end)
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "This error payload is a closed tag union. It cannot gain the additional variants in the function's error type, even when all its variants are listed there. The two unions can have different runtime representations.")
			(line-break)
			(line-break)
			(reflow "Use an explicit")
			(reflow " ")
			(annotated code "match")
			(reflow " ")
			(reflow "to reconstruct each error variant in the return type, or wrap the original error in a tag of that type. Reconstructed payloads must also match the expected payload types. Forwarding the unchanged error payload does not convert it."))))
~~~
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,Comma,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,Comma,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,NoSpaceOpQuestion,
UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,Comma,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,Comma,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,OpFatArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,
CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,Comma,OpenSquare,UpperIdent,CloseSquare,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,Comma,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,NoSpaceOpQuestion,
UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,Comma,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,Comma,OpenSquare,UpperIdent,Comma,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,NoSpaceOpQuestion,
UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "forward")
			(ty-fn
				(ty-apply
					(ty (name "Try"))
					(ty-record)
					(ty-tag-union
						(tags
							(ty-apply
								(ty (name "StdoutErr"))
								(ty (name "Str"))))))
				(ty-apply
					(ty (name "Try"))
					(ty-record)
					(ty-tag-union
						(tags
							(ty-apply
								(ty (name "Exit"))
								(ty (name "I32")))
							(ty-apply
								(ty (name "StdoutErr"))
								(ty (name "Str"))))))))
		(s-decl
			(p-ident (raw "forward"))
			(e-lambda
				(args
					(p-ident (raw "result")))
				(e-block
					(statements
						(e-question-suffix
							(e-ident (raw "result")))
						(e-apply
							(e-tag (raw "Ok"))
							(e-record))))))
		(s-type-anno (name "reconstruct")
			(ty-fn
				(ty-apply
					(ty (name "Try"))
					(ty-record)
					(ty-tag-union
						(tags
							(ty-apply
								(ty (name "StdoutErr"))
								(ty (name "Str"))))))
				(ty-apply
					(ty (name "Try"))
					(ty-record)
					(ty-tag-union
						(tags
							(ty-apply
								(ty (name "Exit"))
								(ty (name "I32")))
							(ty-apply
								(ty (name "StdoutErr"))
								(ty (name "Str"))))))))
		(s-decl
			(p-ident (raw "reconstruct"))
			(e-lambda
				(args
					(p-ident (raw "result")))
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
								(p-tag (raw "StdoutErr")
									(p-ident (raw "message"))))
							(e-apply
								(e-tag (raw "Err"))
								(e-apply
									(e-tag (raw "StdoutErr"))
									(e-ident (raw "message")))))))))
		(s-type-anno (name "disjoint")
			(ty-fn
				(ty-apply
					(ty (name "Try"))
					(ty-record)
					(ty-tag-union
						(tags
							(ty (name "Other")))))
				(ty-apply
					(ty (name "Try"))
					(ty-record)
					(ty-tag-union
						(tags
							(ty-apply
								(ty (name "Exit"))
								(ty (name "I32"))))))))
		(s-decl
			(p-ident (raw "disjoint"))
			(e-lambda
				(args
					(p-ident (raw "result")))
				(e-block
					(statements
						(e-question-suffix
							(e-ident (raw "result")))
						(e-apply
							(e-tag (raw "Ok"))
							(e-record))))))
		(s-type-anno (name "payload_mismatch")
			(ty-fn
				(ty-apply
					(ty (name "Try"))
					(ty-record)
					(ty-tag-union
						(tags
							(ty-apply
								(ty (name "Problem"))
								(ty (name "Str"))))))
				(ty-apply
					(ty (name "Try"))
					(ty-record)
					(ty-tag-union
						(tags
							(ty (name "Extra"))
							(ty-apply
								(ty (name "Problem"))
								(ty (name "I32"))))))))
		(s-decl
			(p-ident (raw "payload_mismatch"))
			(e-lambda
				(args
					(p-ident (raw "result")))
				(e-block
					(statements
						(e-question-suffix
							(e-ident (raw "result")))
						(e-apply
							(e-tag (raw "Ok"))
							(e-record))))))))
~~~
# FORMATTED
~~~roc
forward : Try({}, [StdoutErr(Str)]) -> Try({}, [Exit(I32), StdoutErr(Str)])
forward = |result| {
	result?
	Ok({})
}

# Reconstructing the variant at the return type is accepted.
reconstruct : Try({}, [StdoutErr(Str)]) -> Try({}, [Exit(I32), StdoutErr(Str)])
reconstruct = |result| match result {
	Ok(value) => Ok(value)
	Err(StdoutErr(message)) => Err(StdoutErr(message))
}

# A disjoint error row should retain the ordinary mismatch hint.
disjoint : Try({}, [Other]) -> Try({}, [Exit(I32)])
disjoint = |result| {
	result?
	Ok({})
}

# Matching tag names do not imply matching payload types.
payload_mismatch : Try({}, [Problem(Str)]) -> Try({}, [Extra, Problem(I32)])
payload_mismatch = |result| {
	result?
	Ok({})
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "forward"))
		(e-lambda
			(args
				(p-assign (ident "result")))
			(e-block
				(s-expr
					(e-match
						(match
							(cond
								(e-lookup-local
									(p-assign (ident "result"))))
							(branches
								(branch
									(patterns
										(pattern (degenerate false)
											(p-nominal-external (builtin)
												(p-applied-tag))))
									(value
										(e-lookup-local
											(p-assign (ident "#ok")))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-nominal-external (builtin)
												(p-applied-tag))))
									(value
										(e-return
											(e-runtime-error (tag "erroneous_value_expr")))))))))
				(e-tag (name "Ok")
					(args
						(e-empty_record)))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "Try") (builtin)
					(ty-record)
					(ty-tag-union
						(ty-tag-name (name "StdoutErr")
							(ty-lookup (name "Str") (builtin)))))
				(ty-apply (name "Try") (builtin)
					(ty-record)
					(ty-tag-union
						(ty-tag-name (name "Exit")
							(ty-lookup (name "I32") (builtin)))
						(ty-tag-name (name "StdoutErr")
							(ty-lookup (name "Str") (builtin))))))))
	(d-let
		(p-assign (ident "reconstruct"))
		(e-lambda
			(args
				(p-assign (ident "result")))
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
										(e-tag (name "StdoutErr")
											(args
												(e-lookup-local
													(p-assign (ident "message")))))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "Try") (builtin)
					(ty-record)
					(ty-tag-union
						(ty-tag-name (name "StdoutErr")
							(ty-lookup (name "Str") (builtin)))))
				(ty-apply (name "Try") (builtin)
					(ty-record)
					(ty-tag-union
						(ty-tag-name (name "Exit")
							(ty-lookup (name "I32") (builtin)))
						(ty-tag-name (name "StdoutErr")
							(ty-lookup (name "Str") (builtin))))))))
	(d-let
		(p-assign (ident "disjoint"))
		(e-lambda
			(args
				(p-assign (ident "result")))
			(e-block
				(s-expr
					(e-match
						(match
							(cond
								(e-lookup-local
									(p-assign (ident "result"))))
							(branches
								(branch
									(patterns
										(pattern (degenerate false)
											(p-nominal-external (builtin)
												(p-applied-tag))))
									(value
										(e-lookup-local
											(p-assign (ident "#ok")))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-nominal-external (builtin)
												(p-applied-tag))))
									(value
										(e-return
											(e-runtime-error (tag "erroneous_value_expr")))))))))
				(e-tag (name "Ok")
					(args
						(e-empty_record)))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "Try") (builtin)
					(ty-record)
					(ty-tag-union
						(ty-tag-name (name "Other"))))
				(ty-apply (name "Try") (builtin)
					(ty-record)
					(ty-tag-union
						(ty-tag-name (name "Exit")
							(ty-lookup (name "I32") (builtin))))))))
	(d-let
		(p-assign (ident "payload_mismatch"))
		(e-lambda
			(args
				(p-assign (ident "result")))
			(e-block
				(s-expr
					(e-match
						(match
							(cond
								(e-lookup-local
									(p-assign (ident "result"))))
							(branches
								(branch
									(patterns
										(pattern (degenerate false)
											(p-nominal-external (builtin)
												(p-applied-tag))))
									(value
										(e-lookup-local
											(p-assign (ident "#ok")))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-nominal-external (builtin)
												(p-applied-tag))))
									(value
										(e-return
											(e-runtime-error (tag "erroneous_value_expr")))))))))
				(e-tag (name "Ok")
					(args
						(e-empty_record)))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "Try") (builtin)
					(ty-record)
					(ty-tag-union
						(ty-tag-name (name "Problem")
							(ty-lookup (name "Str") (builtin)))))
				(ty-apply (name "Try") (builtin)
					(ty-record)
					(ty-tag-union
						(ty-tag-name (name "Extra"))
						(ty-tag-name (name "Problem")
							(ty-lookup (name "I32") (builtin)))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Try({}, [StdoutErr(Str)]) -> Try({}, [Exit(I32), StdoutErr(Str)])"))
		(patt (type "Try({}, [StdoutErr(Str)]) -> Try({}, [Exit(I32), StdoutErr(Str)])"))
		(patt (type "Try({}, [Other]) -> Try({}, [Exit(I32)])"))
		(patt (type "Try({}, [Problem(Str)]) -> Try({}, [Extra, Problem(I32)])")))
	(expressions
		(expr (type "Try({}, [StdoutErr(Str)]) -> Try({}, [Exit(I32), StdoutErr(Str)])"))
		(expr (type "Try({}, [StdoutErr(Str)]) -> Try({}, [Exit(I32), StdoutErr(Str)])"))
		(expr (type "Try({}, [Other]) -> Try({}, [Exit(I32)])"))
		(expr (type "Try({}, [Problem(Str)]) -> Try({}, [Extra, Problem(I32)])"))))
~~~

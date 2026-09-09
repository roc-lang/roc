# META
~~~ini
description=A ? applied to the value a function returns warns, even where it type-checks by unwrapping a nested Try
type=snippet
~~~
# SOURCE
~~~roc
# A `?` applied to a function's return value only type-checks when the Ok
# payload is itself a Try, and even then it is confusing to read, so it warns.
nested : Str -> Try(Try(Str, [Bad]), [Bad])
nested = |s| if s == "" Err(Bad) else Ok(Ok(s))

tail : Str -> Try(Str, [Bad])
tail = |s| {
	nested(s)?
}

returned : Str -> Try(Str, [Bad])
returned = |s| {
	if s == "x" {
		return nested(s)?
	}
	if s == "y" nested(s)? else Ok(s)
}
~~~
# EXPECTED
TRAILING `?` - try_suffix_trailing_warning.md:8:11:8:12
TRAILING `?` - try_suffix_trailing_warning.md:14:19:14:20
TRAILING `?` - try_suffix_trailing_warning.md:16:23:16:24
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Trailing `?`")
		(region (start 8 11) (end 8 12))
		(headline
			(reflow "It's usually a mistake to use a postfix ")
			(annotated code "?")
			(reflow " on values being returned implicitly at the end of a function like this:"))
		(document
			(source-region (file "try_suffix_trailing_warning.md") (start 8 11) (end 8 12) (annotation warning) (line-text "\tnested(s)?"))
			(line-break)
			(reflow "This is because ")
			(annotated code "?")
			(reflow " is syntax sugar for doing a ")
			(annotated code "match")
			(reflow " on a ")
			(annotated code "Try")
			(reflow " value like this:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(annotated keyword "match")
			(text " ")
			(annotated symbol-unqualified "value_before_question_mark")
			(text " {")
			(line-break)
			(indent 2)
			(annotated tag "Ok")
			(text "(")
			(annotated symbol-unqualified "ok_payload")
			(text ") ")
			(annotated operator "=>")
			(text " ")
			(annotated symbol-unqualified "ok_payload")
			(line-break)
			(indent 2)
			(annotated tag "Err")
			(text "(")
			(annotated symbol-unqualified "err_payload")
			(text ") ")
			(annotated operator "=>")
			(text " ")
			(annotated keyword "return")
			(text " ")
			(annotated tag "Err")
			(text "(")
			(annotated symbol-unqualified "err_payload")
			(text ")")
			(line-break)
			(indent 1)
			(text "}")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "When you use ")
			(annotated code "?")
			(reflow " on the value at the end of a function, it changes \"implicitly return this ")
			(annotated code "Try")
			(reflow " value\" to \"return this ")
			(annotated code "Try")
			(reflow " value if it's an ")
			(annotated code "Err")
			(reflow ", but if it's ")
			(annotated code "Ok")
			(reflow ", unwrap its ")
			(annotated code "Ok")
			(reflow " payload and return that instead\" - which can only possibly type-check when returning ")
			(annotated code "Try(Try(..., ...), ...)")
			(reflow ", which is so unusual that using ")
			(annotated code "?")
			(reflow " here is almost always a mistake in practice.")
			(line-break)
			(line-break)
			(reflow "Usually removing the ")
			(annotated code "?")
			(reflow " here is what makes the most sense, but if you really want this behavior, make it clear by using an explicit ")
			(annotated code "match")
			(reflow " instead of the ")
			(annotated code "?")
			(reflow " syntax sugar.")))
	(report
		(severity warning)
		(title "Trailing `?`")
		(region (start 14 19) (end 14 20))
		(headline
			(reflow "It's usually a mistake to use a postfix ")
			(annotated code "?")
			(reflow " on values being returned implicitly at the end of a function like this:"))
		(document
			(source-region (file "try_suffix_trailing_warning.md") (start 14 19) (end 14 20) (annotation warning) (line-text "\t\treturn nested(s)?"))
			(line-break)
			(reflow "This is because ")
			(annotated code "?")
			(reflow " is syntax sugar for doing a ")
			(annotated code "match")
			(reflow " on a ")
			(annotated code "Try")
			(reflow " value like this:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(annotated keyword "match")
			(text " ")
			(annotated symbol-unqualified "value_before_question_mark")
			(text " {")
			(line-break)
			(indent 2)
			(annotated tag "Ok")
			(text "(")
			(annotated symbol-unqualified "ok_payload")
			(text ") ")
			(annotated operator "=>")
			(text " ")
			(annotated symbol-unqualified "ok_payload")
			(line-break)
			(indent 2)
			(annotated tag "Err")
			(text "(")
			(annotated symbol-unqualified "err_payload")
			(text ") ")
			(annotated operator "=>")
			(text " ")
			(annotated keyword "return")
			(text " ")
			(annotated tag "Err")
			(text "(")
			(annotated symbol-unqualified "err_payload")
			(text ")")
			(line-break)
			(indent 1)
			(text "}")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "When you use ")
			(annotated code "?")
			(reflow " on the value at the end of a function, it changes \"implicitly return this ")
			(annotated code "Try")
			(reflow " value\" to \"return this ")
			(annotated code "Try")
			(reflow " value if it's an ")
			(annotated code "Err")
			(reflow ", but if it's ")
			(annotated code "Ok")
			(reflow ", unwrap its ")
			(annotated code "Ok")
			(reflow " payload and return that instead\" - which can only possibly type-check when returning ")
			(annotated code "Try(Try(..., ...), ...)")
			(reflow ", which is so unusual that using ")
			(annotated code "?")
			(reflow " here is almost always a mistake in practice.")
			(line-break)
			(line-break)
			(reflow "Usually removing the ")
			(annotated code "?")
			(reflow " here is what makes the most sense, but if you really want this behavior, make it clear by using an explicit ")
			(annotated code "match")
			(reflow " instead of the ")
			(annotated code "?")
			(reflow " syntax sugar.")))
	(report
		(severity warning)
		(title "Trailing `?`")
		(region (start 16 23) (end 16 24))
		(headline
			(reflow "It's usually a mistake to use a postfix ")
			(annotated code "?")
			(reflow " on values being returned implicitly at the end of a function like this:"))
		(document
			(source-region (file "try_suffix_trailing_warning.md") (start 16 23) (end 16 24) (annotation warning) (line-text "\tif s == \"y\" nested(s)? else Ok(s)"))
			(line-break)
			(reflow "This is because ")
			(annotated code "?")
			(reflow " is syntax sugar for doing a ")
			(annotated code "match")
			(reflow " on a ")
			(annotated code "Try")
			(reflow " value like this:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(annotated keyword "match")
			(text " ")
			(annotated symbol-unqualified "value_before_question_mark")
			(text " {")
			(line-break)
			(indent 2)
			(annotated tag "Ok")
			(text "(")
			(annotated symbol-unqualified "ok_payload")
			(text ") ")
			(annotated operator "=>")
			(text " ")
			(annotated symbol-unqualified "ok_payload")
			(line-break)
			(indent 2)
			(annotated tag "Err")
			(text "(")
			(annotated symbol-unqualified "err_payload")
			(text ") ")
			(annotated operator "=>")
			(text " ")
			(annotated keyword "return")
			(text " ")
			(annotated tag "Err")
			(text "(")
			(annotated symbol-unqualified "err_payload")
			(text ")")
			(line-break)
			(indent 1)
			(text "}")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "When you use ")
			(annotated code "?")
			(reflow " on the value at the end of a function, it changes \"implicitly return this ")
			(annotated code "Try")
			(reflow " value\" to \"return this ")
			(annotated code "Try")
			(reflow " value if it's an ")
			(annotated code "Err")
			(reflow ", but if it's ")
			(annotated code "Ok")
			(reflow ", unwrap its ")
			(annotated code "Ok")
			(reflow " payload and return that instead\" - which can only possibly type-check when returning ")
			(annotated code "Try(Try(..., ...), ...)")
			(reflow ", which is so unusual that using ")
			(annotated code "?")
			(reflow " here is almost always a mistake in practice.")
			(line-break)
			(line-break)
			(reflow "Usually removing the ")
			(annotated code "?")
			(reflow " here is what makes the most sense, but if you really want this behavior, make it clear by using an explicit ")
			(annotated code "match")
			(reflow " instead of the ")
			(annotated code "?")
			(reflow " syntax sugar."))))
~~~
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,CloseSquare,CloseRound,Comma,OpenSquare,UpperIdent,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwIf,LowerIdent,OpEquals,StringStart,StringPart,StringEnd,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,KwElse,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceOpQuestion,
CloseCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
KwIf,LowerIdent,OpEquals,StringStart,StringPart,StringEnd,OpenCurly,
KwReturn,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceOpQuestion,
CloseCurly,
KwIf,LowerIdent,OpEquals,StringStart,StringPart,StringEnd,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceOpQuestion,KwElse,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "nested")
			(ty-fn
				(ty (name "Str"))
				(ty-apply
					(ty (name "Try"))
					(ty-apply
						(ty (name "Try"))
						(ty (name "Str"))
						(ty-tag-union
							(tags
								(ty (name "Bad")))))
					(ty-tag-union
						(tags
							(ty (name "Bad")))))))
		(s-decl
			(p-ident (raw "nested"))
			(e-lambda
				(args
					(p-ident (raw "s")))
				(e-if-then-else
					(e-binop (op "==")
						(e-ident (raw "s"))
						(e-string
							(e-string-part (raw ""))))
					(e-apply
						(e-tag (raw "Err"))
						(e-tag (raw "Bad")))
					(e-apply
						(e-tag (raw "Ok"))
						(e-apply
							(e-tag (raw "Ok"))
							(e-ident (raw "s")))))))
		(s-type-anno (name "tail")
			(ty-fn
				(ty (name "Str"))
				(ty-apply
					(ty (name "Try"))
					(ty (name "Str"))
					(ty-tag-union
						(tags
							(ty (name "Bad")))))))
		(s-decl
			(p-ident (raw "tail"))
			(e-lambda
				(args
					(p-ident (raw "s")))
				(e-block
					(statements
						(e-question-suffix
							(e-apply
								(e-ident (raw "nested"))
								(e-ident (raw "s"))))))))
		(s-type-anno (name "returned")
			(ty-fn
				(ty (name "Str"))
				(ty-apply
					(ty (name "Try"))
					(ty (name "Str"))
					(ty-tag-union
						(tags
							(ty (name "Bad")))))))
		(s-decl
			(p-ident (raw "returned"))
			(e-lambda
				(args
					(p-ident (raw "s")))
				(e-block
					(statements
						(e-if-without-else
							(e-binop (op "==")
								(e-ident (raw "s"))
								(e-string
									(e-string-part (raw "x"))))
							(e-block
								(statements
									(s-return
										(e-question-suffix
											(e-apply
												(e-ident (raw "nested"))
												(e-ident (raw "s"))))))))
						(e-if-then-else
							(e-binop (op "==")
								(e-ident (raw "s"))
								(e-string
									(e-string-part (raw "y"))))
							(e-question-suffix
								(e-apply
									(e-ident (raw "nested"))
									(e-ident (raw "s"))))
							(e-apply
								(e-tag (raw "Ok"))
								(e-ident (raw "s"))))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "nested"))
		(e-lambda
			(args
				(p-assign (ident "s")))
			(e-if
				(if-branches
					(if-branch
						(e-method-eq (negated "false")
							(lhs
								(e-lookup-local
									(p-assign (ident "s"))))
							(rhs
								(e-string
									(e-literal (string "")))))
						(e-tag (name "Err")
							(args
								(e-tag (name "Bad"))))))
				(if-else
					(e-tag (name "Ok")
						(args
							(e-tag (name "Ok")
								(args
									(e-lookup-local
										(p-assign (ident "s"))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-apply (name "Try") (builtin)
					(ty-apply (name "Try") (builtin)
						(ty-lookup (name "Str") (builtin))
						(ty-tag-union
							(ty-tag-name (name "Bad"))))
					(ty-tag-union
						(ty-tag-name (name "Bad")))))))
	(d-let
		(p-assign (ident "tail"))
		(e-lambda
			(args
				(p-assign (ident "s")))
			(e-block
				(e-match
					(match
						(cond
							(e-call (constraint-fn-var 451)
								(e-lookup-local
									(p-assign (ident "nested")))
								(e-lookup-local
									(p-assign (ident "s")))))
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
										(e-nominal-external
											(builtin)
											(e-tag (name "Err")
												(args
													(e-lookup-local
														(p-assign (ident "#err"))))))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "Str") (builtin))
					(ty-tag-union
						(ty-tag-name (name "Bad")))))))
	(d-let
		(p-assign (ident "returned"))
		(e-lambda
			(args
				(p-assign (ident "s")))
			(e-block
				(s-expr
					(e-if
						(if-branches
							(if-branch
								(e-method-eq (negated "false")
									(lhs
										(e-lookup-local
											(p-assign (ident "s"))))
									(rhs
										(e-string
											(e-literal (string "x")))))
								(e-block
									(e-return
										(e-match
											(match
												(cond
													(e-call (constraint-fn-var 526)
														(e-lookup-local
															(p-assign (ident "nested")))
														(e-lookup-local
															(p-assign (ident "s")))))
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
																(e-nominal-external
																	(builtin)
																	(e-tag (name "Err")
																		(args
																			(e-lookup-local
																				(p-assign (ident "#err"))))))))))))))))
						(if-else
							(e-empty_record))))
				(e-if
					(if-branches
						(if-branch
							(e-method-eq (negated "false")
								(lhs
									(e-lookup-local
										(p-assign (ident "s"))))
								(rhs
									(e-string
										(e-literal (string "y")))))
							(e-match
								(match
									(cond
										(e-call (constraint-fn-var 595)
											(e-lookup-local
												(p-assign (ident "nested")))
											(e-lookup-local
												(p-assign (ident "s")))))
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
													(e-nominal-external
														(builtin)
														(e-tag (name "Err")
															(args
																(e-lookup-local
																	(p-assign (ident "#err"))))))))))))))
					(if-else
						(e-tag (name "Ok")
							(args
								(e-lookup-local
									(p-assign (ident "s")))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "Str") (builtin))
					(ty-tag-union
						(ty-tag-name (name "Bad"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str -> Try(Try(Str, [Bad]), [Bad])"))
		(patt (type "Str -> Try(Str, [Bad])"))
		(patt (type "Str -> Try(Str, [Bad])")))
	(expressions
		(expr (type "Str -> Try(Try(Str, [Bad]), [Bad])"))
		(expr (type "Str -> Try(Str, [Bad])"))
		(expr (type "Str -> Try(Str, [Bad])"))))
~~~

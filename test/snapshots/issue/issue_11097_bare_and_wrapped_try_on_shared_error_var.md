# META
~~~ini
description=A bare and tag-wrapped try suffix can share a callee error variable
type=snippet
~~~
# SOURCE
~~~roc
run = |save| {
	_ = save("a")?
	_ = save("b") ? PersistFailed
	Ok({})
}

main = run(|_key| Ok({}))
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
Underscore,OpAssign,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,NoSpaceOpQuestion,
Underscore,OpAssign,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,OpQuestion,UpperIdent,
UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,OpBar,NamedUnderscore,OpBar,UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "run"))
			(e-lambda
				(args
					(p-ident (raw "save")))
				(e-block
					(statements
						(s-decl
							(p-underscore)
							(e-question-suffix
								(e-apply
									(e-ident (raw "save"))
									(e-string
										(e-string-part (raw "a"))))))
						(s-decl
							(p-underscore)
							(e-binop (op "?")
								(e-apply
									(e-ident (raw "save"))
									(e-string
										(e-string-part (raw "b"))))
								(e-tag (raw "PersistFailed"))))
						(e-apply
							(e-tag (raw "Ok"))
							(e-record))))))
		(s-decl
			(p-ident (raw "main"))
			(e-apply
				(e-ident (raw "run"))
				(e-lambda
					(args
						(p-ident (raw "_key")))
					(e-apply
						(e-tag (raw "Ok"))
						(e-record)))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "run"))
		(e-lambda
			(args
				(p-assign (ident "save")))
			(e-block
				(s-let
					(p-underscore)
					(e-match
						(match
							(cond
								(e-call (constraint-fn-var 271)
									(e-lookup-local
										(p-assign (ident "save")))
									(e-string
										(e-literal (string "a")))))
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
															(p-assign (ident "#err")))))))))))))
				(s-let
					(p-underscore)
					(e-match
						(match
							(cond
								(e-call (constraint-fn-var 318)
									(e-lookup-local
										(p-assign (ident "save")))
									(e-string
										(e-literal (string "b")))))
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
														(e-tag (name "PersistFailed")
															(args
																(e-lookup-local
																	(p-assign (ident "#err")))))))))))))))
				(e-tag (name "Ok")
					(args
						(e-empty_record))))))
	(d-let
		(p-assign (ident "main"))
		(e-call (constraint-fn-var 386)
			(e-lookup-local
				(p-assign (ident "run")))
			(e-lambda
				(args
					(p-assign (ident "_key")))
				(e-tag (name "Ok")
					(args
						(e-empty_record)))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "(a -> Try(ok, err)) -> Try({}, [PersistFailed(err), ..err]) where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)])]"))
		(patt (type "Try({}, [PersistFailed(err), ..err])")))
	(expressions
		(expr (type "(a -> Try(ok, err)) -> Try({}, [PersistFailed(err), ..err]) where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)])]"))
		(expr (type "Try({}, [PersistFailed(err), ..err])"))))
~~~

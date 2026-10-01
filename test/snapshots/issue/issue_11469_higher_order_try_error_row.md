# META
~~~ini
description=A tag-wrapped try suffix composed into a higher-order function's return row leaves its callback's error row free
type=snippet
~~~
# SOURCE
~~~roc
# repro for https://github.com/roc-lang/roc/issues/11469
# `transaction` wraps one `?` error and returns `operation`'s `Try` in tail
# position, so `operation`'s error row is INCLUDED IN the composed return row
# rather than equal to it. A caller that passes `execute` and a callback whose
# errors come from that same `execute` must therefore check.
transaction = |execute, operation| {
	_ = execute("BEGIN") ? BeginFailed
	operation({})
}

save = |execute|
	transaction(
		execute,
		|{}| {
			_ = execute("INSERT")?
			Ok({})
		},
	)

main = save(|_sql| Ok({}))

# The callback's own `?` is incidental: forwarding `execute`'s `Try` directly
# must check too.
forwarding_save = |execute| transaction(execute, |{}| execute("INSERT"))
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,
Underscore,OpAssign,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,OpQuestion,UpperIdent,
LowerIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,
LowerIdent,NoSpaceOpenRound,
LowerIdent,Comma,
OpBar,OpenCurly,CloseCurly,OpBar,OpenCurly,
Underscore,OpAssign,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,NoSpaceOpQuestion,
UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
CloseCurly,Comma,
CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,OpBar,NamedUnderscore,OpBar,UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceOpenRound,LowerIdent,Comma,OpBar,OpenCurly,CloseCurly,OpBar,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "transaction"))
			(e-lambda
				(args
					(p-ident (raw "execute"))
					(p-ident (raw "operation")))
				(e-block
					(statements
						(s-decl
							(p-underscore)
							(e-binop (op "?")
								(e-apply
									(e-ident (raw "execute"))
									(e-string
										(e-string-part (raw "BEGIN"))))
								(e-tag (raw "BeginFailed"))))
						(e-apply
							(e-ident (raw "operation"))
							(e-record))))))
		(s-decl
			(p-ident (raw "save"))
			(e-lambda
				(args
					(p-ident (raw "execute")))
				(e-apply
					(e-ident (raw "transaction"))
					(e-ident (raw "execute"))
					(e-lambda
						(args
							(p-record))
						(e-block
							(statements
								(s-decl
									(p-underscore)
									(e-question-suffix
										(e-apply
											(e-ident (raw "execute"))
											(e-string
												(e-string-part (raw "INSERT"))))))
								(e-apply
									(e-tag (raw "Ok"))
									(e-record))))))))
		(s-decl
			(p-ident (raw "main"))
			(e-apply
				(e-ident (raw "save"))
				(e-lambda
					(args
						(p-ident (raw "_sql")))
					(e-apply
						(e-tag (raw "Ok"))
						(e-record)))))
		(s-decl
			(p-ident (raw "forwarding_save"))
			(e-lambda
				(args
					(p-ident (raw "execute")))
				(e-apply
					(e-ident (raw "transaction"))
					(e-ident (raw "execute"))
					(e-lambda
						(args
							(p-record))
						(e-apply
							(e-ident (raw "execute"))
							(e-string
								(e-string-part (raw "INSERT"))))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "transaction"))
		(e-lambda
			(args
				(p-assign (ident "execute"))
				(p-assign (ident "operation")))
			(e-block
				(s-let
					(p-underscore)
					(e-match
						(match
							(cond
								(e-call (constraint-fn-var 306)
									(e-lookup-local
										(p-assign (ident "execute")))
									(e-string
										(e-literal (string "BEGIN")))))
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
														(e-tag (name "BeginFailed")
															(args
																(e-lookup-local
																	(p-assign (ident "#err")))))))))))))))
				(e-call (constraint-fn-var 353)
					(e-lookup-local
						(p-assign (ident "operation")))
					(e-empty_record)))))
	(d-let
		(p-assign (ident "save"))
		(e-lambda
			(args
				(p-assign (ident "execute")))
			(e-call (constraint-fn-var 430)
				(e-lookup-local
					(p-assign (ident "transaction")))
				(e-lookup-local
					(p-assign (ident "execute")))
				(e-closure
					(captures
						(capture (ident "execute")))
					(e-lambda
						(args
							(p-record-destructure
								(destructs)))
						(e-block
							(s-let
								(p-underscore)
								(e-match
									(match
										(cond
											(e-call (constraint-fn-var 386)
												(e-lookup-local
													(p-assign (ident "execute")))
												(e-string
													(e-literal (string "INSERT")))))
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
							(e-tag (name "Ok")
								(args
									(e-empty_record)))))))))
	(d-let
		(p-assign (ident "main"))
		(e-call (constraint-fn-var 450)
			(e-lookup-local
				(p-assign (ident "save")))
			(e-lambda
				(args
					(p-assign (ident "_sql")))
				(e-tag (name "Ok")
					(args
						(e-empty_record))))))
	(d-let
		(p-assign (ident "forwarding_save"))
		(e-lambda
			(args
				(p-assign (ident "execute")))
			(e-call (constraint-fn-var 480)
				(e-lookup-local
					(p-assign (ident "transaction")))
				(e-lookup-local
					(p-assign (ident "execute")))
				(e-closure
					(captures
						(capture (ident "execute")))
					(e-lambda
						(args
							(p-record-destructure
								(destructs)))
						(e-call (constraint-fn-var 479)
							(e-lookup-local
								(p-assign (ident "execute")))
							(e-string
								(e-literal (string "INSERT"))))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "(a -> Try(ok, err)), ({} -> Try(ok, b)) -> Try(ok, [BeginFailed(err), ..b]) where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)])]"))
		(patt (type "(a -> Try(ok, err)) -> Try({}, [BeginFailed(err), ..err]) where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)])]"))
		(patt (type "Try({}, [BeginFailed(err), ..err])"))
		(patt (type "(a -> Try(ok, err)) -> Try(ok, [BeginFailed(err), ..err]) where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)])]")))
	(expressions
		(expr (type "(a -> Try(ok, err)), ({} -> Try(ok, b)) -> Try(ok, [BeginFailed(err), ..b]) where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)])]"))
		(expr (type "(a -> Try(ok, err)) -> Try({}, [BeginFailed(err), ..err]) where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)])]"))
		(expr (type "Try({}, [BeginFailed(err), ..err])"))
		(expr (type "(a -> Try(ok, err)) -> Try(ok, [BeginFailed(err), ..err]) where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)])]"))))
~~~

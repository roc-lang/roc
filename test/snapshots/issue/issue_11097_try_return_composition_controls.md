# META
~~~ini
description=Try return-row composition preserves the non-recursive neighboring cases
type=snippet
~~~
# SOURCE
~~~roc
two_bare = |save| {
	_ = save("a")?
	_ = save("b")?
	Ok({})
}

two_wrapped = |save| {
	_ = save("a") ? WrappedA
	_ = save("b") ? WrappedB
	Ok({})
}

mixed_reversed = |save| {
	_ = save("a") ? PersistFailed
	_ = save("b")?
	Ok({})
}

mixed_distinct = |first, second| {
	_ = first("a")?
	_ = second("b") ? PersistFailed
	Ok({})
}

mapped_wrapper = |save| {
	_ = save("a")?
	_ = save("b").map_err(|err| PersistFailed(err))?
	Ok({})
}

annotated : (Str -> Try({}, e)) -> Try({}, [PersistFailed(e), ..e])
annotated = |save| {
	_ = save("a")?
	_ = save("b") ? PersistFailed
	Ok({})
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
Underscore,OpAssign,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,NoSpaceOpQuestion,
Underscore,OpAssign,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,NoSpaceOpQuestion,
UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
Underscore,OpAssign,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,OpQuestion,UpperIdent,
Underscore,OpAssign,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,OpQuestion,UpperIdent,
UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
Underscore,OpAssign,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,OpQuestion,UpperIdent,
Underscore,OpAssign,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,NoSpaceOpQuestion,
UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,
Underscore,OpAssign,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,NoSpaceOpQuestion,
Underscore,OpAssign,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,OpQuestion,UpperIdent,
UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
Underscore,OpAssign,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,NoSpaceOpQuestion,
Underscore,OpAssign,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,NoSpaceDotLowerIdent,NoSpaceOpenRound,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,NoSpaceOpQuestion,
UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
CloseCurly,
LowerIdent,OpColon,OpenRound,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,Comma,LowerIdent,CloseRound,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,Comma,OpenSquare,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,DoubleDot,LowerIdent,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
Underscore,OpAssign,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,NoSpaceOpQuestion,
Underscore,OpAssign,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,OpQuestion,UpperIdent,
UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "two_bare"))
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
							(e-question-suffix
								(e-apply
									(e-ident (raw "save"))
									(e-string
										(e-string-part (raw "b"))))))
						(e-apply
							(e-tag (raw "Ok"))
							(e-record))))))
		(s-decl
			(p-ident (raw "two_wrapped"))
			(e-lambda
				(args
					(p-ident (raw "save")))
				(e-block
					(statements
						(s-decl
							(p-underscore)
							(e-binop (op "?")
								(e-apply
									(e-ident (raw "save"))
									(e-string
										(e-string-part (raw "a"))))
								(e-tag (raw "WrappedA"))))
						(s-decl
							(p-underscore)
							(e-binop (op "?")
								(e-apply
									(e-ident (raw "save"))
									(e-string
										(e-string-part (raw "b"))))
								(e-tag (raw "WrappedB"))))
						(e-apply
							(e-tag (raw "Ok"))
							(e-record))))))
		(s-decl
			(p-ident (raw "mixed_reversed"))
			(e-lambda
				(args
					(p-ident (raw "save")))
				(e-block
					(statements
						(s-decl
							(p-underscore)
							(e-binop (op "?")
								(e-apply
									(e-ident (raw "save"))
									(e-string
										(e-string-part (raw "a"))))
								(e-tag (raw "PersistFailed"))))
						(s-decl
							(p-underscore)
							(e-question-suffix
								(e-apply
									(e-ident (raw "save"))
									(e-string
										(e-string-part (raw "b"))))))
						(e-apply
							(e-tag (raw "Ok"))
							(e-record))))))
		(s-decl
			(p-ident (raw "mixed_distinct"))
			(e-lambda
				(args
					(p-ident (raw "first"))
					(p-ident (raw "second")))
				(e-block
					(statements
						(s-decl
							(p-underscore)
							(e-question-suffix
								(e-apply
									(e-ident (raw "first"))
									(e-string
										(e-string-part (raw "a"))))))
						(s-decl
							(p-underscore)
							(e-binop (op "?")
								(e-apply
									(e-ident (raw "second"))
									(e-string
										(e-string-part (raw "b"))))
								(e-tag (raw "PersistFailed"))))
						(e-apply
							(e-tag (raw "Ok"))
							(e-record))))))
		(s-decl
			(p-ident (raw "mapped_wrapper"))
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
							(e-question-suffix
								(e-method-call (method ".map_err")
									(receiver
										(e-apply
											(e-ident (raw "save"))
											(e-string
												(e-string-part (raw "b")))))
									(args
										(e-lambda
											(args
												(p-ident (raw "err")))
											(e-apply
												(e-tag (raw "PersistFailed"))
												(e-ident (raw "err"))))))))
						(e-apply
							(e-tag (raw "Ok"))
							(e-record))))))
		(s-type-anno (name "annotated")
			(ty-fn
				(ty-fn
					(ty (name "Str"))
					(ty-apply
						(ty (name "Try"))
						(ty-record)
						(ty-var (raw "e"))))
				(ty-apply
					(ty (name "Try"))
					(ty-record)
					(ty-tag-union
						(tags
							(ty-apply
								(ty (name "PersistFailed"))
								(ty-var (raw "e"))))
						(ty-var (raw "e"))))))
		(s-decl
			(p-ident (raw "annotated"))
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
		(p-assign (ident "two_bare"))
		(e-lambda
			(args
				(p-assign (ident "save")))
			(e-block
				(s-let
					(p-underscore)
					(e-match
						(match
							(cond
								(e-call (constraint-fn-var 564)
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
								(e-call (constraint-fn-var 608)
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
														(e-lookup-local
															(p-assign (ident "#err")))))))))))))
				(e-tag (name "Ok")
					(args
						(e-empty_record))))))
	(d-let
		(p-assign (ident "two_wrapped"))
		(e-lambda
			(args
				(p-assign (ident "save")))
			(e-block
				(s-let
					(p-underscore)
					(e-match
						(match
							(cond
								(e-call (constraint-fn-var 661)
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
														(e-tag (name "WrappedA")
															(args
																(e-lookup-local
																	(p-assign (ident "#err")))))))))))))))
				(s-let
					(p-underscore)
					(e-match
						(match
							(cond
								(e-call (constraint-fn-var 710)
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
														(e-tag (name "WrappedB")
															(args
																(e-lookup-local
																	(p-assign (ident "#err")))))))))))))))
				(e-tag (name "Ok")
					(args
						(e-empty_record))))))
	(d-let
		(p-assign (ident "mixed_reversed"))
		(e-lambda
			(args
				(p-assign (ident "save")))
			(e-block
				(s-let
					(p-underscore)
					(e-match
						(match
							(cond
								(e-call (constraint-fn-var 771)
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
														(e-tag (name "PersistFailed")
															(args
																(e-lookup-local
																	(p-assign (ident "#err")))))))))))))))
				(s-let
					(p-underscore)
					(e-match
						(match
							(cond
								(e-call (constraint-fn-var 820)
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
														(e-lookup-local
															(p-assign (ident "#err")))))))))))))
				(e-tag (name "Ok")
					(args
						(e-empty_record))))))
	(d-let
		(p-assign (ident "mixed_distinct"))
		(e-lambda
			(args
				(p-assign (ident "first"))
				(p-assign (ident "second")))
			(e-block
				(s-let
					(p-underscore)
					(e-match
						(match
							(cond
								(e-call (constraint-fn-var 873)
									(e-lookup-local
										(p-assign (ident "first")))
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
								(e-call (constraint-fn-var 920)
									(e-lookup-local
										(p-assign (ident "second")))
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
		(p-assign (ident "mapped_wrapper"))
		(e-lambda
			(args
				(p-assign (ident "save")))
			(e-block
				(s-let
					(p-underscore)
					(e-match
						(match
							(cond
								(e-call (constraint-fn-var 978)
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
								(e-dispatch-call (method "map_err") (constraint-fn-var 1024)
									(receiver
										(e-call (constraint-fn-var 1022)
											(e-lookup-local
												(p-assign (ident "save")))
											(e-string
												(e-literal (string "b")))))
									(args
										(e-lambda
											(args
												(p-assign (ident "err")))
											(e-tag (name "PersistFailed")
												(args
													(e-lookup-local
														(p-assign (ident "err")))))))))
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
						(e-empty_record))))))
	(d-let
		(p-assign (ident "annotated"))
		(e-lambda
			(args
				(p-assign (ident "save")))
			(e-block
				(s-let
					(p-underscore)
					(e-match
						(match
							(cond
								(e-call (constraint-fn-var 1109)
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
								(e-call (constraint-fn-var 1153)
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
						(e-empty_record)))))
		(annotation
			(ty-fn (effectful false)
				(ty-parens
					(ty-fn (effectful false)
						(ty-lookup (name "Str") (builtin))
						(ty-apply (name "Try") (builtin)
							(ty-record)
							(ty-rigid-var (name "e")))))
				(ty-apply (name "Try") (builtin)
					(ty-record)
					(ty-tag-union
						(ty-tag-name (name "PersistFailed")
							(ty-rigid-var-lookup (ty-rigid-var (name "e"))))
						(ty-rigid-var-lookup (ty-rigid-var (name "e")))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "(c -> Try(ok, err)) -> Try({}, err) where [c.from_quote : Str -> Try(c, [BadQuotedBytes(Str)])]"))
		(patt (type "(c -> Try(ok, err)) -> Try({}, [WrappedA(err), WrappedB(err), ..]) where [c.from_quote : Str -> Try(c, [BadQuotedBytes(Str)])]"))
		(patt (type "(c -> Try(ok, err)) -> Try({}, [PersistFailed(err), ..err]) where [c.from_quote : Str -> Try(c, [BadQuotedBytes(Str)])]"))
		(patt (type "(c -> Try(ok, [PersistFailed(err), ..d])), (f -> Try(ok, err)) -> Try({}, [PersistFailed(err), ..d]) where [c.from_quote : Str -> Try(c, [BadQuotedBytes(Str)]), f.from_quote : Str -> Try(f, [BadQuotedBytes(Str)])]"))
		(patt (type "(c -> Try(ok, a)) -> Try({}, [PersistFailed(a), ..a]) where [c.from_quote : Str -> Try(c, [BadQuotedBytes(Str)])]"))
		(patt (type "(Str -> Try({}, e)) -> Try({}, [PersistFailed(e), ..e])")))
	(expressions
		(expr (type "(c -> Try(ok, err)) -> Try({}, err) where [c.from_quote : Str -> Try(c, [BadQuotedBytes(Str)])]"))
		(expr (type "(c -> Try(ok, err)) -> Try({}, [WrappedA(err), WrappedB(err), ..]) where [c.from_quote : Str -> Try(c, [BadQuotedBytes(Str)])]"))
		(expr (type "(c -> Try(ok, err)) -> Try({}, [PersistFailed(err), ..err]) where [c.from_quote : Str -> Try(c, [BadQuotedBytes(Str)])]"))
		(expr (type "(c -> Try(ok, [PersistFailed(err), ..d])), (f -> Try(ok, err)) -> Try({}, [PersistFailed(err), ..d]) where [c.from_quote : Str -> Try(c, [BadQuotedBytes(Str)]), f.from_quote : Str -> Try(f, [BadQuotedBytes(Str)])]"))
		(expr (type "(c -> Try(ok, a)) -> Try({}, [PersistFailed(a), ..a]) where [c.from_quote : Str -> Try(c, [BadQuotedBytes(Str)])]"))
		(expr (type "(Str -> Try({}, e)) -> Try({}, [PersistFailed(e), ..e])"))))
~~~

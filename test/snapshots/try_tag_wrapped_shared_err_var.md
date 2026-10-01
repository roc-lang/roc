# META
~~~ini
description=A bare ? and a tag-wrapped ? on calls sharing one error type variable, where the callee adds a tag of its own, check without inferring an anonymous recursive type
type=snippet
~~~
# SOURCE
~~~roc
# repro for https://github.com/roc-lang/roc/issues/11470

# `find` propagates the error of `query` and adds a tag of its own.
find = |query| {
	rows = query("SELECT")?
	if rows == 0 { Err(NotFound) } else { Ok(rows) }
}

# `query` is a parameter, so both `find` calls share one error type variable.
show = |query| {
	listing = find(query)?
	share = find(query) ? ShareLookupFailed
	Ok(listing + share)
}

run = |{}| {
	_ = show(|_sql| Ok(1))
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
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,NoSpaceOpQuestion,
KwIf,LowerIdent,OpEquals,Int,OpenCurly,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseCurly,KwElse,OpenCurly,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseCurly,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceOpQuestion,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpQuestion,UpperIdent,
UpperIdent,NoSpaceOpenRound,LowerIdent,OpPlus,LowerIdent,CloseRound,
CloseCurly,
LowerIdent,OpAssign,OpBar,OpenCurly,CloseCurly,OpBar,OpenCurly,
Underscore,OpAssign,LowerIdent,NoSpaceOpenRound,OpBar,NamedUnderscore,OpBar,UpperIdent,NoSpaceOpenRound,Int,CloseRound,CloseRound,
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
			(p-ident (raw "find"))
			(e-lambda
				(args
					(p-ident (raw "query")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "rows"))
							(e-question-suffix
								(e-apply
									(e-ident (raw "query"))
									(e-string
										(e-string-part (raw "SELECT"))))))
						(e-if-then-else
							(e-binop (op "==")
								(e-ident (raw "rows"))
								(e-int (raw "0")))
							(e-block
								(statements
									(e-apply
										(e-tag (raw "Err"))
										(e-tag (raw "NotFound")))))
							(e-block
								(statements
									(e-apply
										(e-tag (raw "Ok"))
										(e-ident (raw "rows"))))))))))
		(s-decl
			(p-ident (raw "show"))
			(e-lambda
				(args
					(p-ident (raw "query")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "listing"))
							(e-question-suffix
								(e-apply
									(e-ident (raw "find"))
									(e-ident (raw "query")))))
						(s-decl
							(p-ident (raw "share"))
							(e-binop (op "?")
								(e-apply
									(e-ident (raw "find"))
									(e-ident (raw "query")))
								(e-tag (raw "ShareLookupFailed"))))
						(e-apply
							(e-tag (raw "Ok"))
							(e-binop (op "+")
								(e-ident (raw "listing"))
								(e-ident (raw "share"))))))))
		(s-decl
			(p-ident (raw "run"))
			(e-lambda
				(args
					(p-record))
				(e-block
					(statements
						(s-decl
							(p-underscore)
							(e-apply
								(e-ident (raw "show"))
								(e-lambda
									(args
										(p-ident (raw "_sql")))
									(e-apply
										(e-tag (raw "Ok"))
										(e-int (raw "1"))))))
						(e-apply
							(e-tag (raw "Ok"))
							(e-record))))))))
~~~
# FORMATTED
~~~roc
# repro for https://github.com/roc-lang/roc/issues/11470

# `find` propagates the error of `query` and adds a tag of its own.
find = |query| {
	rows = query("SELECT")?
	if rows == 0 {
		Err(NotFound)
	} else {
		Ok(rows)
	}
}

# `query` is a parameter, so both `find` calls share one error type variable.
show = |query| {
	listing = find(query)?
	share = find(query) ? ShareLookupFailed
	Ok(listing + share)
}

run = |{}| {
	_ = show(|_sql| Ok(1))
	Ok({})
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "find"))
		(e-lambda
			(args
				(p-assign (ident "query")))
			(e-block
				(s-let
					(p-assign (ident "rows"))
					(e-match
						(match
							(cond
								(e-call (constraint-fn-var 320)
									(e-lookup-local
										(p-assign (ident "query")))
									(e-string
										(e-literal (string "SELECT")))))
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
				(e-if
					(if-branches
						(if-branch
							(e-method-eq (negated "false")
								(lhs
									(e-lookup-local
										(p-assign (ident "rows"))))
								(rhs
									(e-num (value "0"))))
							(e-block
								(e-tag (name "Err")
									(args
										(e-tag (name "NotFound")))))))
					(if-else
						(e-block
							(e-tag (name "Ok")
								(args
									(e-lookup-local
										(p-assign (ident "rows")))))))))))
	(d-let
		(p-assign (ident "show"))
		(e-lambda
			(args
				(p-assign (ident "query")))
			(e-block
				(s-let
					(p-assign (ident "listing"))
					(e-match
						(match
							(cond
								(e-call (constraint-fn-var 395)
									(e-lookup-local
										(p-assign (ident "find")))
									(e-lookup-local
										(p-assign (ident "query")))))
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
					(p-assign (ident "share"))
					(e-match
						(match
							(cond
								(e-call (constraint-fn-var 444)
									(e-lookup-local
										(p-assign (ident "find")))
									(e-lookup-local
										(p-assign (ident "query")))))
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
														(e-tag (name "ShareLookupFailed")
															(args
																(e-lookup-local
																	(p-assign (ident "#err")))))))))))))))
				(e-tag (name "Ok")
					(args
						(e-dispatch-call (method "plus") (constraint-fn-var 485)
							(receiver
								(e-lookup-local
									(p-assign (ident "listing"))))
							(args
								(e-lookup-local
									(p-assign (ident "share"))))))))))
	(d-let
		(p-assign (ident "run"))
		(e-lambda
			(args
				(p-record-destructure
					(destructs)))
			(e-block
				(s-let
					(p-underscore)
					(e-call (constraint-fn-var 534)
						(e-lookup-local
							(p-assign (ident "show")))
						(e-lambda
							(args
								(p-assign (ident "_sql")))
							(e-tag (name "Ok")
								(args
									(e-num (value "1")))))))
				(e-tag (name "Ok")
					(args
						(e-empty_record)))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "(a -> Try(ok, [NotFound, ..b])) -> Try(ok, [NotFound, ..b]) where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)]), ok.from_numeral : Numeral -> Try(ok, [InvalidNumeral(Str)]), ok.is_eq : ok, ok -> Bool]"))
		(patt (type "(a -> Try(ok, [NotFound, ..b])) -> Try(ok, [NotFound, ShareLookupFailed([NotFound, ..b]), ..b]) where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)]), ok.from_numeral : Numeral -> Try(ok, [InvalidNumeral(Str)]), ok.is_eq : ok, ok -> Bool, ok.plus : ok, ok -> ok]"))
		(patt (type "{} -> [Ok({})]")))
	(expressions
		(expr (type "(a -> Try(ok, [NotFound, ..b])) -> Try(ok, [NotFound, ..b]) where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)]), ok.from_numeral : Numeral -> Try(ok, [InvalidNumeral(Str)]), ok.is_eq : ok, ok -> Bool]"))
		(expr (type "(a -> Try(ok, [NotFound, ..b])) -> Try(ok, [NotFound, ShareLookupFailed([NotFound, ..b]), ..b]) where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)]), ok.from_numeral : Numeral -> Try(ok, [InvalidNumeral(Str)]), ok.is_eq : ok, ok -> Bool, ok.plus : ok, ok -> ok]"))
		(expr (type "{} -> [Ok({})]"))))
~~~

# META
~~~ini
description=Tagged shared error rows preserve source identity across order, nesting, annotations, and instantiation
type=snippet
~~~
# SOURCE
~~~roc
find = |query| {
	rows = query("SELECT")?
	if rows == 0 { Err(NotFound) } else { Ok(rows) }
}

reversed = |query| {
	share = find(query) ? ShareLookupFailed
	listing = find(query)?
	Ok(listing + share)
}

nested = |query| {
	listing = find(query)?
	share = find(query) ? |err| ShareLookupFailed({ cause: err })
	Ok(listing + share)
}

repeated = |query| {
	first = find(query)?
	second = find(query) ? ShareLookupFailed
	third = find(query)?
	Ok(first + second + third)
}

annotated : (Str -> Try(U64, [NotFound, ..e])) -> Try(U64, [NotFound, ShareLookupFailed([NotFound, ..e]), ..e])
annotated = |query| {
	listing = query("a")?
	share = query("b") ? ShareLookupFailed
	Ok(listing + share)
}

# Both wrapping contributions identify their payloads. This exposes tags in
# the first bare contribution only while the return rows are being composed.
exposed_tail = |first, query| {
	_ = first({})?
	_ = find(query)?
	_ = first({}) ? Wrapped
	_ = find(query) ? Wrapped
	Ok({})
}

run = |{}| {
	_ = reversed(|_| Ok(1.U64))
	_ = nested(|_| Err(QueryFailed))
	_ = repeated(|_| Ok(1.U64))
	_ = annotated(|_| Err(NotFound))
	_ = exposed_tail(|_| Err(First), |_| Err(Second))
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
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpQuestion,UpperIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceOpQuestion,
UpperIdent,NoSpaceOpenRound,LowerIdent,OpPlus,LowerIdent,CloseRound,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceOpQuestion,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpQuestion,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceOpenRound,OpenCurly,LowerIdent,OpColon,LowerIdent,CloseCurly,CloseRound,
UpperIdent,NoSpaceOpenRound,LowerIdent,OpPlus,LowerIdent,CloseRound,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceOpQuestion,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpQuestion,UpperIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceOpQuestion,
UpperIdent,NoSpaceOpenRound,LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,CloseRound,
CloseCurly,
LowerIdent,OpColon,OpenRound,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,Comma,DoubleDot,LowerIdent,CloseSquare,CloseRound,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,Comma,UpperIdent,NoSpaceOpenRound,OpenSquare,UpperIdent,Comma,DoubleDot,LowerIdent,CloseSquare,CloseRound,Comma,DoubleDot,LowerIdent,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,NoSpaceOpQuestion,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,OpQuestion,UpperIdent,
UpperIdent,NoSpaceOpenRound,LowerIdent,OpPlus,LowerIdent,CloseRound,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,
Underscore,OpAssign,LowerIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,NoSpaceOpQuestion,
Underscore,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceOpQuestion,
Underscore,OpAssign,LowerIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,OpQuestion,UpperIdent,
Underscore,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpQuestion,UpperIdent,
UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
CloseCurly,
LowerIdent,OpAssign,OpBar,OpenCurly,CloseCurly,OpBar,OpenCurly,
Underscore,OpAssign,LowerIdent,NoSpaceOpenRound,OpBar,Underscore,OpBar,UpperIdent,NoSpaceOpenRound,Int,NoSpaceDotUpperIdent,CloseRound,CloseRound,
Underscore,OpAssign,LowerIdent,NoSpaceOpenRound,OpBar,Underscore,OpBar,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseRound,
Underscore,OpAssign,LowerIdent,NoSpaceOpenRound,OpBar,Underscore,OpBar,UpperIdent,NoSpaceOpenRound,Int,NoSpaceDotUpperIdent,CloseRound,CloseRound,
Underscore,OpAssign,LowerIdent,NoSpaceOpenRound,OpBar,Underscore,OpBar,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseRound,
Underscore,OpAssign,LowerIdent,NoSpaceOpenRound,OpBar,Underscore,OpBar,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,Comma,OpBar,Underscore,OpBar,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseRound,
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
			(p-ident (raw "reversed"))
			(e-lambda
				(args
					(p-ident (raw "query")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "share"))
							(e-binop (op "?")
								(e-apply
									(e-ident (raw "find"))
									(e-ident (raw "query")))
								(e-tag (raw "ShareLookupFailed"))))
						(s-decl
							(p-ident (raw "listing"))
							(e-question-suffix
								(e-apply
									(e-ident (raw "find"))
									(e-ident (raw "query")))))
						(e-apply
							(e-tag (raw "Ok"))
							(e-binop (op "+")
								(e-ident (raw "listing"))
								(e-ident (raw "share"))))))))
		(s-decl
			(p-ident (raw "nested"))
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
								(e-lambda
									(args
										(p-ident (raw "err")))
									(e-apply
										(e-tag (raw "ShareLookupFailed"))
										(e-record
											(field (field "cause")
												(e-ident (raw "err"))))))))
						(e-apply
							(e-tag (raw "Ok"))
							(e-binop (op "+")
								(e-ident (raw "listing"))
								(e-ident (raw "share"))))))))
		(s-decl
			(p-ident (raw "repeated"))
			(e-lambda
				(args
					(p-ident (raw "query")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "first"))
							(e-question-suffix
								(e-apply
									(e-ident (raw "find"))
									(e-ident (raw "query")))))
						(s-decl
							(p-ident (raw "second"))
							(e-binop (op "?")
								(e-apply
									(e-ident (raw "find"))
									(e-ident (raw "query")))
								(e-tag (raw "ShareLookupFailed"))))
						(s-decl
							(p-ident (raw "third"))
							(e-question-suffix
								(e-apply
									(e-ident (raw "find"))
									(e-ident (raw "query")))))
						(e-apply
							(e-tag (raw "Ok"))
							(e-binop (op "+")
								(e-binop (op "+")
									(e-ident (raw "first"))
									(e-ident (raw "second")))
								(e-ident (raw "third"))))))))
		(s-type-anno (name "annotated")
			(ty-fn
				(ty-fn
					(ty (name "Str"))
					(ty-apply
						(ty (name "Try"))
						(ty (name "U64"))
						(ty-tag-union
							(tags
								(ty (name "NotFound")))
							(ty-var (raw "e")))))
				(ty-apply
					(ty (name "Try"))
					(ty (name "U64"))
					(ty-tag-union
						(tags
							(ty (name "NotFound"))
							(ty-apply
								(ty (name "ShareLookupFailed"))
								(ty-tag-union
									(tags
										(ty (name "NotFound")))
									(ty-var (raw "e")))))
						(ty-var (raw "e"))))))
		(s-decl
			(p-ident (raw "annotated"))
			(e-lambda
				(args
					(p-ident (raw "query")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "listing"))
							(e-question-suffix
								(e-apply
									(e-ident (raw "query"))
									(e-string
										(e-string-part (raw "a"))))))
						(s-decl
							(p-ident (raw "share"))
							(e-binop (op "?")
								(e-apply
									(e-ident (raw "query"))
									(e-string
										(e-string-part (raw "b"))))
								(e-tag (raw "ShareLookupFailed"))))
						(e-apply
							(e-tag (raw "Ok"))
							(e-binop (op "+")
								(e-ident (raw "listing"))
								(e-ident (raw "share"))))))))
		(s-decl
			(p-ident (raw "exposed_tail"))
			(e-lambda
				(args
					(p-ident (raw "first"))
					(p-ident (raw "query")))
				(e-block
					(statements
						(s-decl
							(p-underscore)
							(e-question-suffix
								(e-apply
									(e-ident (raw "first"))
									(e-record))))
						(s-decl
							(p-underscore)
							(e-question-suffix
								(e-apply
									(e-ident (raw "find"))
									(e-ident (raw "query")))))
						(s-decl
							(p-underscore)
							(e-binop (op "?")
								(e-apply
									(e-ident (raw "first"))
									(e-record))
								(e-tag (raw "Wrapped"))))
						(s-decl
							(p-underscore)
							(e-binop (op "?")
								(e-apply
									(e-ident (raw "find"))
									(e-ident (raw "query")))
								(e-tag (raw "Wrapped"))))
						(e-apply
							(e-tag (raw "Ok"))
							(e-record))))))
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
								(e-ident (raw "reversed"))
								(e-lambda
									(args
										(p-underscore))
									(e-apply
										(e-tag (raw "Ok"))
										(e-typed-int (raw "1") (type "U64"))))))
						(s-decl
							(p-underscore)
							(e-apply
								(e-ident (raw "nested"))
								(e-lambda
									(args
										(p-underscore))
									(e-apply
										(e-tag (raw "Err"))
										(e-tag (raw "QueryFailed"))))))
						(s-decl
							(p-underscore)
							(e-apply
								(e-ident (raw "repeated"))
								(e-lambda
									(args
										(p-underscore))
									(e-apply
										(e-tag (raw "Ok"))
										(e-typed-int (raw "1") (type "U64"))))))
						(s-decl
							(p-underscore)
							(e-apply
								(e-ident (raw "annotated"))
								(e-lambda
									(args
										(p-underscore))
									(e-apply
										(e-tag (raw "Err"))
										(e-tag (raw "NotFound"))))))
						(s-decl
							(p-underscore)
							(e-apply
								(e-ident (raw "exposed_tail"))
								(e-lambda
									(args
										(p-underscore))
									(e-apply
										(e-tag (raw "Err"))
										(e-tag (raw "First"))))
								(e-lambda
									(args
										(p-underscore))
									(e-apply
										(e-tag (raw "Err"))
										(e-tag (raw "Second"))))))
						(e-apply
							(e-tag (raw "Ok"))
							(e-record))))))))
~~~
# FORMATTED
~~~roc
find = |query| {
	rows = query("SELECT")?
	if rows == 0 {
		Err(NotFound)
	} else {
		Ok(rows)
	}
}

reversed = |query| {
	share = find(query) ? ShareLookupFailed
	listing = find(query)?
	Ok(listing + share)
}

nested = |query| {
	listing = find(query)?
	share = find(query) ? |err| ShareLookupFailed({ cause: err })
	Ok(listing + share)
}

repeated = |query| {
	first = find(query)?
	second = find(query) ? ShareLookupFailed
	third = find(query)?
	Ok(first + second + third)
}

annotated : (Str -> Try(U64, [NotFound, ..e])) -> Try(U64, [NotFound, ShareLookupFailed([NotFound, ..e]), ..e])
annotated = |query| {
	listing = query("a")?
	share = query("b") ? ShareLookupFailed
	Ok(listing + share)
}

# Both wrapping contributions identify their payloads. This exposes tags in
# the first bare contribution only while the return rows are being composed.
exposed_tail = |first, query| {
	_ = first({})?
	_ = find(query)?
	_ = first({}) ? Wrapped
	_ = find(query) ? Wrapped
	Ok({})
}

run = |{}| {
	_ = reversed(|_| Ok(1.U64))
	_ = nested(|_| Err(QueryFailed))
	_ = repeated(|_| Ok(1.U64))
	_ = annotated(|_| Err(NotFound))
	_ = exposed_tail(|_| Err(First), |_| Err(Second))
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
								(e-call (constraint-fn-var 672)
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
		(p-assign (ident "reversed"))
		(e-lambda
			(args
				(p-assign (ident "query")))
			(e-block
				(s-let
					(p-assign (ident "share"))
					(e-match
						(match
							(cond
								(e-call (constraint-fn-var 742)
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
				(s-let
					(p-assign (ident "listing"))
					(e-match
						(match
							(cond
								(e-call (constraint-fn-var 796)
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
				(e-tag (name "Ok")
					(args
						(e-dispatch-call (method "plus") (constraint-fn-var 832)
							(receiver
								(e-lookup-local
									(p-assign (ident "listing"))))
							(args
								(e-lookup-local
									(p-assign (ident "share"))))))))))
	(d-let
		(p-assign (ident "nested"))
		(e-lambda
			(args
				(p-assign (ident "query")))
			(e-block
				(s-let
					(p-assign (ident "listing"))
					(e-match
						(match
							(cond
								(e-call (constraint-fn-var 859)
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
								(e-call (constraint-fn-var 908)
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
														(e-call (constraint-fn-var 945)
															(e-lambda
																(args
																	(p-assign (ident "err")))
																(e-tag (name "ShareLookupFailed")
																	(args
																		(e-record
																			(fields
																				(field (name "cause")
																					(e-lookup-local
																						(p-assign (ident "err")))))))))
															(e-lookup-local
																(p-assign (ident "#err"))))))))))))))
				(e-tag (name "Ok")
					(args
						(e-dispatch-call (method "plus") (constraint-fn-var 948)
							(receiver
								(e-lookup-local
									(p-assign (ident "listing"))))
							(args
								(e-lookup-local
									(p-assign (ident "share"))))))))))
	(d-let
		(p-assign (ident "repeated"))
		(e-lambda
			(args
				(p-assign (ident "query")))
			(e-block
				(s-let
					(p-assign (ident "first"))
					(e-match
						(match
							(cond
								(e-call (constraint-fn-var 975)
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
					(p-assign (ident "second"))
					(e-match
						(match
							(cond
								(e-call (constraint-fn-var 1024)
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
				(s-let
					(p-assign (ident "third"))
					(e-match
						(match
							(cond
								(e-call (constraint-fn-var 1078)
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
				(e-tag (name "Ok")
					(args
						(e-dispatch-call (method "plus") (constraint-fn-var 1116)
							(receiver
								(e-dispatch-call (method "plus") (constraint-fn-var 1114)
									(receiver
										(e-lookup-local
											(p-assign (ident "first"))))
									(args
										(e-lookup-local
											(p-assign (ident "second"))))))
							(args
								(e-lookup-local
									(p-assign (ident "third"))))))))))
	(d-let
		(p-assign (ident "annotated"))
		(e-lambda
			(args
				(p-assign (ident "query")))
			(e-block
				(s-let
					(p-assign (ident "listing"))
					(e-match
						(match
							(cond
								(e-call (constraint-fn-var 1154)
									(e-lookup-local
										(p-assign (ident "query")))
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
					(p-assign (ident "share"))
					(e-match
						(match
							(cond
								(e-call (constraint-fn-var 1198)
									(e-lookup-local
										(p-assign (ident "query")))
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
														(e-tag (name "ShareLookupFailed")
															(args
																(e-lookup-local
																	(p-assign (ident "#err")))))))))))))))
				(e-tag (name "Ok")
					(args
						(e-dispatch-call (method "plus") (constraint-fn-var 1239)
							(receiver
								(e-lookup-local
									(p-assign (ident "listing"))))
							(args
								(e-lookup-local
									(p-assign (ident "share")))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-parens
					(ty-fn (effectful false)
						(ty-lookup (name "Str") (builtin))
						(ty-apply (name "Try") (builtin)
							(ty-lookup (name "U64") (builtin))
							(ty-tag-union
								(ty-tag-name (name "NotFound"))
								(ty-rigid-var (name "e"))))))
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "U64") (builtin))
					(ty-tag-union
						(ty-tag-name (name "NotFound"))
						(ty-tag-name (name "ShareLookupFailed")
							(ty-tag-union
								(ty-tag-name (name "NotFound"))
								(ty-rigid-var-lookup (ty-rigid-var (name "e")))))
						(ty-rigid-var-lookup (ty-rigid-var (name "e"))))))))
	(d-let
		(p-assign (ident "exposed_tail"))
		(e-lambda
			(args
				(p-assign (ident "first"))
				(p-assign (ident "query")))
			(e-block
				(s-let
					(p-underscore)
					(e-match
						(match
							(cond
								(e-call (constraint-fn-var 1257)
									(e-lookup-local
										(p-assign (ident "first")))
									(e-empty_record)))
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
								(e-call (constraint-fn-var 1306)
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
					(p-underscore)
					(e-match
						(match
							(cond
								(e-call (constraint-fn-var 1342)
									(e-lookup-local
										(p-assign (ident "first")))
									(e-empty_record)))
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
														(e-tag (name "Wrapped")
															(args
																(e-lookup-local
																	(p-assign (ident "#err")))))))))))))))
				(s-let
					(p-underscore)
					(e-match
						(match
							(cond
								(e-call (constraint-fn-var 1396)
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
														(e-tag (name "Wrapped")
															(args
																(e-lookup-local
																	(p-assign (ident "#err")))))))))))))))
				(e-tag (name "Ok")
					(args
						(e-empty_record))))))
	(d-let
		(p-assign (ident "run"))
		(e-lambda
			(args
				(p-record-destructure
					(destructs)))
			(e-block
				(s-let
					(p-underscore)
					(e-call (constraint-fn-var 1487)
						(e-lookup-local
							(p-assign (ident "reversed")))
						(e-lambda
							(args
								(p-underscore))
							(e-tag (name "Ok")
								(args
									(e-typed-int (value "1") (type "U64")))))))
				(s-let
					(p-underscore)
					(e-call (constraint-fn-var 1528)
						(e-lookup-local
							(p-assign (ident "nested")))
						(e-lambda
							(args
								(p-underscore))
							(e-tag (name "Err")
								(args
									(e-tag (name "QueryFailed")))))))
				(s-let
					(p-underscore)
					(e-call (constraint-fn-var 1563)
						(e-lookup-local
							(p-assign (ident "repeated")))
						(e-lambda
							(args
								(p-underscore))
							(e-tag (name "Ok")
								(args
									(e-typed-int (value "1") (type "U64")))))))
				(s-let
					(p-underscore)
					(e-call (constraint-fn-var 1584)
						(e-lookup-local
							(p-assign (ident "annotated")))
						(e-lambda
							(args
								(p-underscore))
							(e-tag (name "Err")
								(args
									(e-tag (name "NotFound")))))))
				(s-let
					(p-underscore)
					(e-call (constraint-fn-var 1627)
						(e-lookup-local
							(p-assign (ident "exposed_tail")))
						(e-lambda
							(args
								(p-underscore))
							(e-tag (name "Err")
								(args
									(e-tag (name "First")))))
						(e-lambda
							(args
								(p-underscore))
							(e-tag (name "Err")
								(args
									(e-tag (name "Second")))))))
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
		(patt (type "(a -> Try(ok, [NotFound, ..b])) -> Try(ok, [NotFound, ShareLookupFailed({ cause: [NotFound, ..b] }), ..b]) where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)]), ok.from_numeral : Numeral -> Try(ok, [InvalidNumeral(Str)]), ok.is_eq : ok, ok -> Bool, ok.plus : ok, ok -> ok]"))
		(patt (type "(a -> Try(ok, [NotFound, ..b])) -> Try(ok, [NotFound, ShareLookupFailed([NotFound, ..b]), ..b]) where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)]), ok.from_numeral : Numeral -> Try(ok, [InvalidNumeral(Str)]), ok.is_eq : ok, ok -> Bool, ok.plus : ok, ok -> ok]"))
		(patt (type "(Str -> Try(U64, [NotFound, ..e])) -> Try(U64, [NotFound, ShareLookupFailed([NotFound, ..e]), ..e])"))
		(patt (type "({} -> Try(ok, [NotFound, ..a])), (b -> Try(ok, [NotFound, ..a])) -> Try({}, [NotFound, Wrapped([NotFound, ..a]), ..a]) where [b.from_quote : Str -> Try(b, [BadQuotedBytes(Str)]), ok.from_numeral : Numeral -> Try(ok, [InvalidNumeral(Str)]), ok.is_eq : ok, ok -> Bool]"))
		(patt (type "{} -> [Ok({})]")))
	(expressions
		(expr (type "(a -> Try(ok, [NotFound, ..b])) -> Try(ok, [NotFound, ..b]) where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)]), ok.from_numeral : Numeral -> Try(ok, [InvalidNumeral(Str)]), ok.is_eq : ok, ok -> Bool]"))
		(expr (type "(a -> Try(ok, [NotFound, ..b])) -> Try(ok, [NotFound, ShareLookupFailed([NotFound, ..b]), ..b]) where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)]), ok.from_numeral : Numeral -> Try(ok, [InvalidNumeral(Str)]), ok.is_eq : ok, ok -> Bool, ok.plus : ok, ok -> ok]"))
		(expr (type "(a -> Try(ok, [NotFound, ..b])) -> Try(ok, [NotFound, ShareLookupFailed({ cause: [NotFound, ..b] }), ..b]) where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)]), ok.from_numeral : Numeral -> Try(ok, [InvalidNumeral(Str)]), ok.is_eq : ok, ok -> Bool, ok.plus : ok, ok -> ok]"))
		(expr (type "(a -> Try(ok, [NotFound, ..b])) -> Try(ok, [NotFound, ShareLookupFailed([NotFound, ..b]), ..b]) where [a.from_quote : Str -> Try(a, [BadQuotedBytes(Str)]), ok.from_numeral : Numeral -> Try(ok, [InvalidNumeral(Str)]), ok.is_eq : ok, ok -> Bool, ok.plus : ok, ok -> ok]"))
		(expr (type "(Str -> Try(U64, [NotFound, ..e])) -> Try(U64, [NotFound, ShareLookupFailed([NotFound, ..e]), ..e])"))
		(expr (type "({} -> Try(ok, [NotFound, ..a])), (b -> Try(ok, [NotFound, ..a])) -> Try({}, [NotFound, Wrapped([NotFound, ..a]), ..a]) where [b.from_quote : Str -> Try(b, [BadQuotedBytes(Str)]), ok.from_numeral : Numeral -> Try(ok, [InvalidNumeral(Str)]), ok.is_eq : ok, ok -> Bool]"))
		(expr (type "{} -> [Ok({})]"))))
~~~

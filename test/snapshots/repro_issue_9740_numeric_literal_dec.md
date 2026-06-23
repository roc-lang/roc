# META
~~~ini
description=repro for https://github.com/roc-lang/roc/issues/9740 — literal 0 in a U64-annotated recursive arg position must unify with U64, not infer Dec
type=file
~~~
# SOURCE
~~~roc
slice : List(a), U64, U64 -> List(a)
slice = |l, start, end| {
	match l {
		[] => []
		[x, .. as rest] => {
			if start > 0 {
				slice(rest, (start - 1), (end - 1))
			} else if end == 0 {
				[x]
			} else {
				List.prepend(slice(rest, 0, (end - 1)), x)
			}
		}
	}
}

main! = |_| {
    Ok({})
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,UpperIdent,Comma,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,
KwMatch,LowerIdent,OpenCurly,
OpenSquare,CloseSquare,OpFatArrow,OpenSquare,CloseSquare,
OpenSquare,LowerIdent,Comma,DoubleDot,KwAs,LowerIdent,CloseSquare,OpFatArrow,OpenCurly,
KwIf,LowerIdent,OpGreaterThan,Int,OpenCurly,
LowerIdent,NoSpaceOpenRound,LowerIdent,Comma,OpenRound,LowerIdent,OpBinaryMinus,Int,CloseRound,Comma,OpenRound,LowerIdent,OpBinaryMinus,Int,CloseRound,CloseRound,
CloseCurly,KwElse,KwIf,LowerIdent,OpEquals,Int,OpenCurly,
OpenSquare,LowerIdent,CloseSquare,
CloseCurly,KwElse,OpenCurly,
UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,NoSpaceOpenRound,LowerIdent,Comma,Int,Comma,OpenRound,LowerIdent,OpBinaryMinus,Int,CloseRound,CloseRound,Comma,LowerIdent,CloseRound,
CloseCurly,
CloseCurly,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "slice")
			(ty-fn
				(ty-apply
					(ty (name "List"))
					(ty-var (raw "a")))
				(ty (name "U64"))
				(ty (name "U64"))
				(ty-apply
					(ty (name "List"))
					(ty-var (raw "a")))))
		(s-decl
			(p-ident (raw "slice"))
			(e-lambda
				(args
					(p-ident (raw "l"))
					(p-ident (raw "start"))
					(p-ident (raw "end")))
				(e-block
					(statements
						(e-match
							(e-ident (raw "l"))
							(branches
								(branch
									(p-list)
									(e-list))
								(branch
									(p-list
										(p-ident (raw "x"))
										(p-list-rest (name "rest")))
									(e-block
										(statements
											(e-if-then-else
												(e-binop (op ">")
													(e-ident (raw "start"))
													(e-int (raw "0")))
												(e-block
													(statements
														(e-apply
															(e-ident (raw "slice"))
															(e-ident (raw "rest"))
															(e-tuple
																(e-binop (op "-")
																	(e-ident (raw "start"))
																	(e-int (raw "1"))))
															(e-tuple
																(e-binop (op "-")
																	(e-ident (raw "end"))
																	(e-int (raw "1")))))))
												(e-if-then-else
													(e-binop (op "==")
														(e-ident (raw "end"))
														(e-int (raw "0")))
													(e-block
														(statements
															(e-list
																(e-ident (raw "x")))))
													(e-block
														(statements
															(e-apply
																(e-ident (raw "List.prepend"))
																(e-apply
																	(e-ident (raw "slice"))
																	(e-ident (raw "rest"))
																	(e-int (raw "0"))
																	(e-tuple
																		(e-binop (op "-")
																			(e-ident (raw "end"))
																			(e-int (raw "1")))))
																(e-ident (raw "x"))))))))))))))))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(e-apply
							(e-tag (raw "Ok"))
							(e-record))))))))
~~~
# FORMATTED
~~~roc
slice : List(a), U64, U64 -> List(a)
slice = |l, start, end| {
	match l {
		[] => []
		[x, .. as rest] => {
			if start > 0 {
				slice(rest, (start - 1), (end - 1))
			} else if end == 0 {
				[x]
			} else {
				List.prepend(slice(rest, 0, (end - 1)), x)
			}
		}
	}
}

main! = |_| {
	Ok({})
}
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
		(p-assign (ident "slice"))
		(e-lambda
			(args
				(p-assign (ident "l"))
				(p-assign (ident "start"))
				(p-assign (ident "end")))
			(e-block
				(e-match
					(match
						(cond
							(e-lookup-local
								(p-assign (ident "l"))))
						(branches
							(branch
								(patterns
									(pattern (degenerate false)
										(p-list
											(patterns))))
								(value
									(e-empty_list)))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-list
											(patterns
												(p-assign (ident "x")))
											(rest-at (index 1)
												(p-assign (ident "rest"))))))
								(value
									(e-block
										(e-if
											(if-branches
												(if-branch
													(e-dispatch-call (method "is_gt") (constraint-fn-var 166)
														(receiver
															(e-lookup-local
																(p-assign (ident "start"))))
														(args
															(e-num (value "0"))))
													(e-block
														(e-call (constraint-fn-var 483)
															(e-lookup-local
																(p-assign (ident "slice")))
															(e-lookup-local
																(p-assign (ident "rest")))
															(e-dispatch-call (method "minus") (constraint-fn-var 288)
																(receiver
																	(e-lookup-local
																		(p-assign (ident "start"))))
																(args
																	(e-num (value "1"))))
															(e-dispatch-call (method "minus") (constraint-fn-var 401)
																(receiver
																	(e-lookup-local
																		(p-assign (ident "end"))))
																(args
																	(e-num (value "1")))))))
												(if-branch
													(e-method-eq (negated "false")
														(lhs
															(e-lookup-local
																(p-assign (ident "end"))))
														(rhs
															(e-num (value "0"))))
													(e-block
														(e-list
															(elems
																(e-lookup-local
																	(p-assign (ident "x"))))))))
											(if-else
												(e-block
													(e-call (constraint-fn-var 776)
														(e-lookup-external
															(builtin))
														(e-call (constraint-fn-var 775)
															(e-lookup-local
																(p-assign (ident "slice")))
															(e-lookup-local
																(p-assign (ident "rest")))
															(e-num (value "0"))
															(e-dispatch-call (method "minus") (constraint-fn-var 693)
																(receiver
																	(e-lookup-local
																		(p-assign (ident "end"))))
																(args
																	(e-num (value "1")))))
														(e-lookup-local
															(p-assign (ident "x")))))))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "List") (builtin)
					(ty-rigid-var (name "a")))
				(ty-lookup (name "U64") (builtin))
				(ty-lookup (name "U64") (builtin))
				(ty-apply (name "List") (builtin)
					(ty-rigid-var-lookup (ty-rigid-var (name "a")))))))
	(d-let
		(p-assign (ident "main!"))
		(e-lambda
			(args
				(p-underscore))
			(e-block
				(e-tag (name "Ok")
					(args
						(e-empty_record)))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str => {}"))
		(patt (type "List(a), U64, U64 -> List(a)"))
		(patt (type "_arg -> [Ok({}), ..]")))
	(expressions
		(expr (type "Str => {}"))
		(expr (type "List(a), U64, U64 -> List(a)"))
		(expr (type "_arg -> [Ok({}), ..]"))))
~~~

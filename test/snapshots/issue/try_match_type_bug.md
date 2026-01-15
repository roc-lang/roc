# META
~~~ini
description=Try return with match and error propagation should type-check
type=snippet
~~~
# SOURCE
~~~roc
get_greeting : {} -> Try(Str, _)
get_greeting = |{}| {
    match 0 {
        0 => Try.Ok(List.first(["hello"])?),
        _ => Err(Impossible)
    }
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,OpenCurly,CloseCurly,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,Underscore,CloseRound,
LowerIdent,OpAssign,OpBar,OpenCurly,CloseCurly,OpBar,OpenCurly,
KwMatch,Int,OpenCurly,
Int,OpFatArrow,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,OpenSquare,StringStart,StringPart,StringEnd,CloseSquare,CloseRound,NoSpaceOpQuestion,CloseRound,Comma,
Underscore,OpFatArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
CloseCurly,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "get_greeting")
			(ty-fn
				(ty-record)
				(ty-apply
					(ty (name "Try"))
					(ty (name "Str"))
					(_))))
		(s-decl
			(p-ident (raw "get_greeting"))
			(e-lambda
				(args
					(p-record))
				(e-block
					(statements
						(e-match
							(e-int (raw "0"))
							(branches
								(branch
									(p-int (raw "0"))
									(e-apply
										(e-tag (raw "Try.Ok"))
										(e-question-suffix
											(e-apply
												(e-ident (raw "List.first"))
												(e-list
													(e-string
														(e-string-part (raw "hello"))))))))
								(branch
									(p-underscore)
									(e-apply
										(e-tag (raw "Err"))
										(e-tag (raw "Impossible"))))))))))))
~~~
# FORMATTED
~~~roc
get_greeting : {} -> Try(Str, _)
get_greeting = |{}| {
	match 0 {
		0 => Try.Ok(List.first(["hello"])?)
		_ => Err(Impossible)
	}
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "get_greeting"))
		(e-lambda
			(args
				(p-record-destructure
					(destructs)))
			(e-block
				(e-match
					(match
						(cond
							(e-num (value "0")))
						(branches
							(branch
								(patterns
									(pattern (degenerate false)
										(p-num (value "0"))))
								(value
									(e-nominal-external
										(builtin)
										(e-tag (name "Ok")
											(args
												(e-match
													(match
														(cond
															(e-call
																(e-lookup-external
																	(builtin))
																(e-list
																	(elems
																		(e-string
																			(e-literal (string "hello")))))))
														(branches
															(branch
																(patterns
																	(pattern (degenerate false)
																		(p-applied-tag)))
																(value
																	(e-lookup-local
																		(p-assign (ident "#ok")))))
															(branch
																(patterns
																	(pattern (degenerate false)
																		(p-applied-tag)))
																(value
																	(e-return
																		(e-tag (name "Err")
																			(args
																				(e-lookup-local
																					(p-assign (ident "#err"))))))))))))))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-underscore)))
								(value
									(e-tag (name "Err")
										(args
											(e-tag (name "Impossible")))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-record)
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "Str") (builtin))
					(ty-underscore))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "{  } -> Try(Str, [ListWasEmpty, Impossible, .._others])")))
	(expressions
		(expr (type "{  } -> Try(Str, [ListWasEmpty, Impossible, .._others])"))))
~~~

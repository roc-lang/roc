# META
~~~ini
description=The single-question binary operator (lhs ? handler) maps the Err payload before early returning; the rhs can be a bare tag constructor or any function-like expression
type=snippet
~~~
# SOURCE
~~~roc
# `? NoFirstError` wraps the err as `NoFirstError(err)` before returning early
question_with_err_tag : List(Str) -> Try(Str, _)
question_with_err_tag = |strings| {
	first_str = strings.first() ? NoFirstError
	Ok(first_str)
}

# `? |e| NoFirstError(e)` is the explicit lambda form
question_with_err_lambda : List(Str) -> Try(Str, _)
question_with_err_lambda = |strings| {
	first_str = strings.first() ? |e| NoFirstError(e)
	Ok(first_str)
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,Underscore,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,OpQuestion,UpperIdent,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,Underscore,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,OpQuestion,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "question_with_err_tag")
			(ty-fn
				(ty-apply
					(ty (name "List"))
					(ty (name "Str")))
				(ty-apply
					(ty (name "Try"))
					(ty (name "Str"))
					(_))))
		(s-decl
			(p-ident (raw "question_with_err_tag"))
			(e-lambda
				(args
					(p-ident (raw "strings")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "first_str"))
							(e-binop (op "?")
								(e-method-call (method ".first")
									(receiver
										(e-ident (raw "strings")))
									(args))
								(e-tag (raw "NoFirstError"))))
						(e-apply
							(e-tag (raw "Ok"))
							(e-ident (raw "first_str")))))))
		(s-type-anno (name "question_with_err_lambda")
			(ty-fn
				(ty-apply
					(ty (name "List"))
					(ty (name "Str")))
				(ty-apply
					(ty (name "Try"))
					(ty (name "Str"))
					(_))))
		(s-decl
			(p-ident (raw "question_with_err_lambda"))
			(e-lambda
				(args
					(p-ident (raw "strings")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "first_str"))
							(e-binop (op "?")
								(e-method-call (method ".first")
									(receiver
										(e-ident (raw "strings")))
									(args))
								(e-lambda
									(args
										(p-ident (raw "e")))
									(e-apply
										(e-tag (raw "NoFirstError"))
										(e-ident (raw "e"))))))
						(e-apply
							(e-tag (raw "Ok"))
							(e-ident (raw "first_str")))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "question_with_err_tag"))
		(e-lambda
			(args
				(p-assign (ident "strings")))
			(e-block
				(s-let
					(p-assign (ident "first_str"))
					(e-match
						(match
							(cond
								(e-dispatch-call (method "first") (constraint-fn-var 120)
									(receiver
										(e-lookup-local
											(p-assign (ident "strings"))))
									(args)))
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
														(e-tag (name "NoFirstError")
															(args
																(e-lookup-local
																	(p-assign (ident "#err")))))))))))))))
				(e-tag (name "Ok")
					(args
						(e-lookup-local
							(p-assign (ident "first_str")))))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "List") (builtin)
					(ty-lookup (name "Str") (builtin)))
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "Str") (builtin))
					(ty-underscore)))))
	(d-let
		(p-assign (ident "question_with_err_lambda"))
		(e-lambda
			(args
				(p-assign (ident "strings")))
			(e-block
				(s-let
					(p-assign (ident "first_str"))
					(e-match
						(match
							(cond
								(e-dispatch-call (method "first") (constraint-fn-var 202)
									(receiver
										(e-lookup-local
											(p-assign (ident "strings"))))
									(args)))
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
														(e-call (constraint-fn-var 249)
															(e-lambda
																(args
																	(p-assign (ident "e")))
																(e-tag (name "NoFirstError")
																	(args
																		(e-lookup-local
																			(p-assign (ident "e"))))))
															(e-lookup-local
																(p-assign (ident "#err"))))))))))))))
				(e-tag (name "Ok")
					(args
						(e-lookup-local
							(p-assign (ident "first_str")))))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "List") (builtin)
					(ty-lookup (name "Str") (builtin)))
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "Str") (builtin))
					(ty-underscore))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "List(Str) -> Try(Str, [NoFirstError([ListWasEmpty, ..]), ..])"))
		(patt (type "List(Str) -> Try(Str, [NoFirstError([ListWasEmpty, ..]), ..])")))
	(expressions
		(expr (type "List(Str) -> Try(Str, [NoFirstError([ListWasEmpty, ..]), ..])"))
		(expr (type "List(Str) -> Try(Str, [NoFirstError([ListWasEmpty, ..]), ..])"))))
~~~

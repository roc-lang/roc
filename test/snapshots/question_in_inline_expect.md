# META
~~~ini
description=? directly inside an inline expect in a function body keeps normal early-return semantics (propagates the Err out of the enclosing function)
type=snippet
~~~
# SOURCE
~~~roc
f : I64 -> Try(I64, [IsNegative])
f = |x| {
	if x < 0 { Err(IsNegative) } else { Ok(x) }
}

g : I64 -> Try(I64, [IsNegative])
g = |x| {
	expect f(x)? == x
	Ok(x)
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
KwIf,LowerIdent,OpLessThan,Int,OpenCurly,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseCurly,KwElse,OpenCurly,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseCurly,
CloseCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
KwExpect,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceOpQuestion,OpEquals,LowerIdent,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "f")
			(ty-fn
				(ty (name "I64"))
				(ty-apply
					(ty (name "Try"))
					(ty (name "I64"))
					(ty-tag-union
						(tags
							(ty (name "IsNegative")))))))
		(s-decl
			(p-ident (raw "f"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-block
					(statements
						(e-if-then-else
							(e-binop (op "<")
								(e-ident (raw "x"))
								(e-int (raw "0")))
							(e-block
								(statements
									(e-apply
										(e-tag (raw "Err"))
										(e-tag (raw "IsNegative")))))
							(e-block
								(statements
									(e-apply
										(e-tag (raw "Ok"))
										(e-ident (raw "x"))))))))))
		(s-type-anno (name "g")
			(ty-fn
				(ty (name "I64"))
				(ty-apply
					(ty (name "Try"))
					(ty (name "I64"))
					(ty-tag-union
						(tags
							(ty (name "IsNegative")))))))
		(s-decl
			(p-ident (raw "g"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-block
					(statements
						(s-expect
							(e-binop (op "==")
								(e-question-suffix
									(e-apply
										(e-ident (raw "f"))
										(e-ident (raw "x"))))
								(e-ident (raw "x"))))
						(e-apply
							(e-tag (raw "Ok"))
							(e-ident (raw "x")))))))))
~~~
# FORMATTED
~~~roc
f : I64 -> Try(I64, [IsNegative])
f = |x| {
	if x < 0 {
		Err(IsNegative)
	} else {
		Ok(x)
	}
}

g : I64 -> Try(I64, [IsNegative])
g = |x| {
	expect f(x)? == x
	Ok(x)
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "f"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-block
				(e-if
					(if-branches
						(if-branch
							(e-dispatch-call (method "is_lt") (constraint-fn-var 130)
								(receiver
									(e-lookup-local
										(p-assign (ident "x"))))
								(args
									(e-num (value "0"))))
							(e-block
								(e-tag (name "Err")
									(args
										(e-tag (name "IsNegative")))))))
					(if-else
						(e-block
							(e-tag (name "Ok")
								(args
									(e-lookup-local
										(p-assign (ident "x"))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "I64") (builtin))
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "I64") (builtin))
					(ty-tag-union
						(ty-tag-name (name "IsNegative")))))))
	(d-let
		(p-assign (ident "g"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-block
				(s-expect
					(e-method-eq (negated "false")
						(lhs
							(e-match
								(match
									(cond
										(e-call (constraint-fn-var 246)
											(e-lookup-local
												(p-assign (ident "f")))
											(e-lookup-local
												(p-assign (ident "x")))))
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
						(rhs
							(e-lookup-local
								(p-assign (ident "x"))))))
				(e-tag (name "Ok")
					(args
						(e-lookup-local
							(p-assign (ident "x")))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "I64") (builtin))
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "I64") (builtin))
					(ty-tag-union
						(ty-tag-name (name "IsNegative"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "I64 -> Try(I64, [IsNegative])"))
		(patt (type "I64 -> Try(I64, [IsNegative])")))
	(expressions
		(expr (type "I64 -> Try(I64, [IsNegative])"))
		(expr (type "I64 -> Try(I64, [IsNegative])"))))
~~~

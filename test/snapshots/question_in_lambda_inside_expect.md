# META
~~~ini
description=? inside a lambda body keeps normal early-return semantics even when the lambda is inside a top-level expect
type=snippet
~~~
# SOURCE
~~~roc
f : I64 -> Try(I64, [IsNegative])
f = |x| {
	if x < 0 { Err(IsNegative) } else { Ok(x) }
}

expect {
	double = |x| {
		value = f(x)?
		Ok(value * 2)
	}
	double(3) == Ok(6)
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
KwExpect,OpenCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceOpQuestion,
UpperIdent,NoSpaceOpenRound,LowerIdent,OpStar,Int,CloseRound,
CloseCurly,
LowerIdent,NoSpaceOpenRound,Int,CloseRound,OpEquals,UpperIdent,NoSpaceOpenRound,Int,CloseRound,
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
		(s-expect
			(e-block
				(statements
					(s-decl
						(p-ident (raw "double"))
						(e-lambda
							(args
								(p-ident (raw "x")))
							(e-block
								(statements
									(s-decl
										(p-ident (raw "value"))
										(e-question-suffix
											(e-apply
												(e-ident (raw "f"))
												(e-ident (raw "x")))))
									(e-apply
										(e-tag (raw "Ok"))
										(e-binop (op "*")
											(e-ident (raw "value"))
											(e-int (raw "2"))))))))
					(e-binop (op "==")
						(e-apply
							(e-ident (raw "double"))
							(e-int (raw "3")))
						(e-apply
							(e-tag (raw "Ok"))
							(e-int (raw "6")))))))))
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

expect {
	double = |x| {
		value = f(x)?
		Ok(value * 2)
	}
	double(3) == Ok(6)
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
							(e-dispatch-call (method "is_lt") (constraint-fn-var 131)
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
	(s-expect
		(e-block
			(s-let
				(p-assign (ident "double"))
				(e-lambda
					(args
						(p-assign (ident "x")))
					(e-block
						(s-let
							(p-assign (ident "value"))
							(e-match
								(match
									(cond
										(e-call (constraint-fn-var 226)
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
						(e-tag (name "Ok")
							(args
								(e-dispatch-call (method "times") (constraint-fn-var 291)
									(receiver
										(e-lookup-local
											(p-assign (ident "value"))))
									(args
										(e-num (value "2")))))))))
			(e-method-eq (negated "false")
				(lhs
					(e-call (constraint-fn-var 406)
						(e-lookup-local
							(p-assign (ident "double")))
						(e-num (value "3"))))
				(rhs
					(e-tag (name "Ok")
						(args
							(e-num (value "6")))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "I64 -> Try(I64, [IsNegative])")))
	(expressions
		(expr (type "I64 -> Try(I64, [IsNegative])"))))
~~~

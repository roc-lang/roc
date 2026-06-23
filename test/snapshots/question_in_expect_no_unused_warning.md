# META
~~~ini
description=Regression test for issue 9612: ? inside a top-level expect must not produce an UNUSED VARIABLE warning for the desugared Err payload
type=snippet
~~~
# SOURCE
~~~roc
f : I64 -> Try(I64, [IsNegative])
f = |x| {
	if x < 0 { Err(IsNegative) } else { Ok(x) }
}

expect {
	result = f(3)?
	result == 3
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
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,CloseRound,NoSpaceOpQuestion,
LowerIdent,OpEquals,Int,
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
						(p-ident (raw "result"))
						(e-question-suffix
							(e-apply
								(e-ident (raw "f"))
								(e-int (raw "3")))))
					(e-binop (op "==")
						(e-ident (raw "result"))
						(e-int (raw "3"))))))))
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
	result = f(3)?
	result == 3
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
							(e-dispatch-call (method "is_lt") (constraint-fn-var 117)
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
				(p-assign (ident "result"))
				(e-match
					(match
						(cond
							(e-call (constraint-fn-var 245)
								(e-lookup-local
									(p-assign (ident "f")))
								(e-num (value "3"))))
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
									(e-expect-err (snippet "f(3)?")
										(e-lookup-local
											(p-assign (ident "#err"))))))))))
			(e-method-eq (negated "false")
				(lhs
					(e-lookup-local
						(p-assign (ident "result"))))
				(rhs
					(e-num (value "3")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "I64 -> Try(I64, [IsNegative])")))
	(expressions
		(expr (type "I64 -> Try(I64, [IsNegative])"))))
~~~

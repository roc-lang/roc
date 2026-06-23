# META
~~~ini
description=? in a top-level definition (outside any expect or lambda) is still an error
type=snippet
~~~
# SOURCE
~~~roc
f : I64 -> Try(I64, [IsNegative])
f = |x| {
	if x < 0 { Err(IsNegative) } else { Ok(x) }
}

result = f(3)?
~~~
# EXPECTED
TRY OPERATOR OUTSIDE FUNCTION - question_in_top_level_def_error.md:6:10:6:15
# PROBLEMS

┌───────────────────────────────┐
│ TRY OPERATOR OUTSIDE FUNCTION ├─ The `?` operator can only be used inside ──┐
└┬──────────────────────────────┘  function bodies because it can cause an    │
 │                                 early return.                              │
 │                                                                            │
 │  result = f(3)?                                                            │
 │           ‾‾‾‾‾                                                            │
 └─────────────────────────────────── question_in_top_level_def_error.md:6:10 ┘


# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
KwIf,LowerIdent,OpLessThan,Int,OpenCurly,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseCurly,KwElse,OpenCurly,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseCurly,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,CloseRound,NoSpaceOpQuestion,
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
		(s-decl
			(p-ident (raw "result"))
			(e-question-suffix
				(e-apply
					(e-ident (raw "f"))
					(e-int (raw "3")))))))
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

result = f(3)?
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
							(e-dispatch-call (method "is_lt") (constraint-fn-var 115)
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
		(p-assign (ident "result"))
		(e-match
			(match
				(cond
					(e-call (constraint-fn-var 243)
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
							(e-runtime-error (tag "return_outside_fn")))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error -> Try(Error, [IsNegative])"))
		(patt (type "Error")))
	(expressions
		(expr (type "Error -> Try(Error, [IsNegative])"))
		(expr (type "Error"))))
~~~

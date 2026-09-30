# META
~~~ini
description=? on the rhs of an annotated local def inside an unannotated function checks its early return against the function's return type, not the local annotation
type=snippet
~~~
# SOURCE
~~~roc
# repro for https://github.com/roc-lang/roc/issues/10798
# `n : U64` annotates the local, so the ? early return must be checked against
# f's return type Try(U64, [BadInput]) rather than against U64.
parse : Str -> Try(U64, [BadInput])
parse = |_| Ok(1)

f = |s| {
	n : U64
	n = parse(s)?
	Ok(n)
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,UpperIdent,NoSpaceOpenRound,Int,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceOpQuestion,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "parse")
			(ty-fn
				(ty (name "Str"))
				(ty-apply
					(ty (name "Try"))
					(ty (name "U64"))
					(ty-tag-union
						(tags
							(ty (name "BadInput")))))))
		(s-decl
			(p-ident (raw "parse"))
			(e-lambda
				(args
					(p-underscore))
				(e-apply
					(e-tag (raw "Ok"))
					(e-int (raw "1")))))
		(s-decl
			(p-ident (raw "f"))
			(e-lambda
				(args
					(p-ident (raw "s")))
				(e-block
					(statements
						(s-type-anno (name "n")
							(ty (name "U64")))
						(s-decl
							(p-ident (raw "n"))
							(e-question-suffix
								(e-apply
									(e-ident (raw "parse"))
									(e-ident (raw "s")))))
						(e-apply
							(e-tag (raw "Ok"))
							(e-ident (raw "n")))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "parse"))
		(e-lambda
			(args
				(p-underscore))
			(e-tag (name "Ok")
				(args
					(e-num (value "1")))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "U64") (builtin))
					(ty-tag-union
						(ty-tag-name (name "BadInput")))))))
	(d-let
		(p-assign (ident "f"))
		(e-lambda
			(args
				(p-assign (ident "s")))
			(e-block
				(s-let
					(p-assign (ident "n"))
					(e-match
						(match
							(cond
								(e-call (constraint-fn-var 281)
									(e-lookup-local
										(p-assign (ident "parse")))
									(e-lookup-local
										(p-assign (ident "s")))))
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
						(e-lookup-local
							(p-assign (ident "n")))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str -> Try(U64, [BadInput])"))
		(patt (type "Str -> Try(U64, [BadInput])")))
	(expressions
		(expr (type "Str -> Try(U64, [BadInput])"))
		(expr (type "Str -> Try(U64, [BadInput])"))))
~~~

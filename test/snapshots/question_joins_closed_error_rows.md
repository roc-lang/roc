# META
~~~ini
description=chained ? sites with different closed error rows join into the enclosing function's error row (design.md "Try Question Row Widening") without widening the callees' own types
type=snippet
~~~
# SOURCE
~~~roc
parse_a : Str -> Try(I64, [BadA])
parse_a = |_| Err(BadA)

parse_b : Str -> Try(I64, [BadB(Str)])
parse_b = |_| Err(BadB("oops"))

combined = |s| {
	a = parse_a(s)?
	b = parse_b(s)?
	Ok(a + b)
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceOpQuestion,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceOpQuestion,
UpperIdent,NoSpaceOpenRound,LowerIdent,OpPlus,LowerIdent,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "parse_a")
			(ty-fn
				(ty (name "Str"))
				(ty-apply
					(ty (name "Try"))
					(ty (name "I64"))
					(ty-tag-union
						(tags
							(ty (name "BadA")))))))
		(s-decl
			(p-ident (raw "parse_a"))
			(e-lambda
				(args
					(p-underscore))
				(e-apply
					(e-tag (raw "Err"))
					(e-tag (raw "BadA")))))
		(s-type-anno (name "parse_b")
			(ty-fn
				(ty (name "Str"))
				(ty-apply
					(ty (name "Try"))
					(ty (name "I64"))
					(ty-tag-union
						(tags
							(ty-apply
								(ty (name "BadB"))
								(ty (name "Str"))))))))
		(s-decl
			(p-ident (raw "parse_b"))
			(e-lambda
				(args
					(p-underscore))
				(e-apply
					(e-tag (raw "Err"))
					(e-apply
						(e-tag (raw "BadB"))
						(e-string
							(e-string-part (raw "oops")))))))
		(s-decl
			(p-ident (raw "combined"))
			(e-lambda
				(args
					(p-ident (raw "s")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "a"))
							(e-question-suffix
								(e-apply
									(e-ident (raw "parse_a"))
									(e-ident (raw "s")))))
						(s-decl
							(p-ident (raw "b"))
							(e-question-suffix
								(e-apply
									(e-ident (raw "parse_b"))
									(e-ident (raw "s")))))
						(e-apply
							(e-tag (raw "Ok"))
							(e-binop (op "+")
								(e-ident (raw "a"))
								(e-ident (raw "b"))))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "parse_a"))
		(e-lambda
			(args
				(p-underscore))
			(e-tag (name "Err")
				(args
					(e-tag (name "BadA")))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "I64") (builtin))
					(ty-tag-union
						(ty-tag-name (name "BadA")))))))
	(d-let
		(p-assign (ident "parse_b"))
		(e-lambda
			(args
				(p-underscore))
			(e-tag (name "Err")
				(args
					(e-tag (name "BadB")
						(args
							(e-string
								(e-literal (string "oops"))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "I64") (builtin))
					(ty-tag-union
						(ty-tag-name (name "BadB")
							(ty-lookup (name "Str") (builtin))))))))
	(d-let
		(p-assign (ident "combined"))
		(e-lambda
			(args
				(p-assign (ident "s")))
			(e-block
				(s-let
					(p-assign (ident "a"))
					(e-match
						(match
							(cond
								(e-call (constraint-fn-var 346)
									(e-lookup-local
										(p-assign (ident "parse_a")))
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
				(s-let
					(p-assign (ident "b"))
					(e-match
						(match
							(cond
								(e-call (constraint-fn-var 382)
									(e-lookup-local
										(p-assign (ident "parse_b")))
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
						(e-dispatch-call (method "plus") (constraint-fn-var 418)
							(receiver
								(e-lookup-local
									(p-assign (ident "a"))))
							(args
								(e-lookup-local
									(p-assign (ident "b")))))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str -> Try(I64, [BadA])"))
		(patt (type "Str -> Try(I64, [BadB(Str)])"))
		(patt (type "Str -> Try(I64, [BadA, BadB(Str), ..])")))
	(expressions
		(expr (type "Str -> Try(I64, [BadA])"))
		(expr (type "Str -> Try(I64, [BadB(Str)])"))
		(expr (type "Str -> Try(I64, [BadA, BadB(Str), ..])"))))
~~~

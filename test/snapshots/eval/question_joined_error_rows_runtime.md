# META
~~~ini
description=re-raised errors from ? sites with different closed rows carry the right tag at runtime through the joined return row (design.md "Try Question Row Widening")
type=snippet
~~~
# SOURCE
~~~roc
parse_a : Str -> Try(I64, [BadA])
parse_a = |s| if s == "a" Ok(1) else Err(BadA)

parse_b : Str -> Try(I64, [BadB(Str)])
parse_b = |s| if s == "b" Ok(2) else Err(BadB(s))

combined = |s1, s2| {
	a = parse_a(s1)?
	b = parse_b(s2)?
	Ok(a + b)
}

expect combined("a", "b") == Ok(3)
expect combined("x", "b") == Err(BadA)
expect combined("a", "x") == Err(BadB("x"))
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwIf,LowerIdent,OpEquals,StringStart,StringPart,StringEnd,UpperIdent,NoSpaceOpenRound,Int,CloseRound,KwElse,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwIf,LowerIdent,OpEquals,StringStart,StringPart,StringEnd,UpperIdent,NoSpaceOpenRound,Int,CloseRound,KwElse,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceOpQuestion,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceOpQuestion,
UpperIdent,NoSpaceOpenRound,LowerIdent,OpPlus,LowerIdent,CloseRound,
CloseCurly,
KwExpect,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,Comma,StringStart,StringPart,StringEnd,CloseRound,OpEquals,UpperIdent,NoSpaceOpenRound,Int,CloseRound,
KwExpect,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,Comma,StringStart,StringPart,StringEnd,CloseRound,OpEquals,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
KwExpect,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,Comma,StringStart,StringPart,StringEnd,CloseRound,OpEquals,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,CloseRound,
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
					(p-ident (raw "s")))
				(e-if-then-else
					(e-binop (op "==")
						(e-ident (raw "s"))
						(e-string
							(e-string-part (raw "a"))))
					(e-apply
						(e-tag (raw "Ok"))
						(e-int (raw "1")))
					(e-apply
						(e-tag (raw "Err"))
						(e-tag (raw "BadA"))))))
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
					(p-ident (raw "s")))
				(e-if-then-else
					(e-binop (op "==")
						(e-ident (raw "s"))
						(e-string
							(e-string-part (raw "b"))))
					(e-apply
						(e-tag (raw "Ok"))
						(e-int (raw "2")))
					(e-apply
						(e-tag (raw "Err"))
						(e-apply
							(e-tag (raw "BadB"))
							(e-ident (raw "s")))))))
		(s-decl
			(p-ident (raw "combined"))
			(e-lambda
				(args
					(p-ident (raw "s1"))
					(p-ident (raw "s2")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "a"))
							(e-question-suffix
								(e-apply
									(e-ident (raw "parse_a"))
									(e-ident (raw "s1")))))
						(s-decl
							(p-ident (raw "b"))
							(e-question-suffix
								(e-apply
									(e-ident (raw "parse_b"))
									(e-ident (raw "s2")))))
						(e-apply
							(e-tag (raw "Ok"))
							(e-binop (op "+")
								(e-ident (raw "a"))
								(e-ident (raw "b"))))))))
		(s-expect
			(e-binop (op "==")
				(e-apply
					(e-ident (raw "combined"))
					(e-string
						(e-string-part (raw "a")))
					(e-string
						(e-string-part (raw "b"))))
				(e-apply
					(e-tag (raw "Ok"))
					(e-int (raw "3")))))
		(s-expect
			(e-binop (op "==")
				(e-apply
					(e-ident (raw "combined"))
					(e-string
						(e-string-part (raw "x")))
					(e-string
						(e-string-part (raw "b"))))
				(e-apply
					(e-tag (raw "Err"))
					(e-tag (raw "BadA")))))
		(s-expect
			(e-binop (op "==")
				(e-apply
					(e-ident (raw "combined"))
					(e-string
						(e-string-part (raw "a")))
					(e-string
						(e-string-part (raw "x"))))
				(e-apply
					(e-tag (raw "Err"))
					(e-apply
						(e-tag (raw "BadB"))
						(e-string
							(e-string-part (raw "x")))))))))
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
				(p-assign (ident "s")))
			(e-if
				(if-branches
					(if-branch
						(e-method-eq (negated "false")
							(lhs
								(e-lookup-local
									(p-assign (ident "s"))))
							(rhs
								(e-string
									(e-literal (string "a")))))
						(e-tag (name "Ok")
							(args
								(e-num (value "1"))))))
				(if-else
					(e-tag (name "Err")
						(args
							(e-tag (name "BadA")))))))
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
				(p-assign (ident "s")))
			(e-if
				(if-branches
					(if-branch
						(e-method-eq (negated "false")
							(lhs
								(e-lookup-local
									(p-assign (ident "s"))))
							(rhs
								(e-string
									(e-literal (string "b")))))
						(e-tag (name "Ok")
							(args
								(e-num (value "2"))))))
				(if-else
					(e-tag (name "Err")
						(args
							(e-tag (name "BadB")
								(args
									(e-lookup-local
										(p-assign (ident "s"))))))))))
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
				(p-assign (ident "s1"))
				(p-assign (ident "s2")))
			(e-block
				(s-let
					(p-assign (ident "a"))
					(e-match
						(match
							(cond
								(e-call (constraint-fn-var 457)
									(e-lookup-local
										(p-assign (ident "parse_a")))
									(e-lookup-local
										(p-assign (ident "s1")))))
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
								(e-call (constraint-fn-var 493)
									(e-lookup-local
										(p-assign (ident "parse_b")))
									(e-lookup-local
										(p-assign (ident "s2")))))
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
						(e-dispatch-call (method "plus") (constraint-fn-var 529)
							(receiver
								(e-lookup-local
									(p-assign (ident "a"))))
							(args
								(e-lookup-local
									(p-assign (ident "b"))))))))))
	(s-expect
		(e-method-eq (negated "false")
			(lhs
				(e-call (constraint-fn-var 571)
					(e-lookup-local
						(p-assign (ident "combined")))
					(e-string
						(e-literal (string "a")))
					(e-string
						(e-literal (string "b")))))
			(rhs
				(e-tag (name "Ok")
					(args
						(e-num (value "3")))))))
	(s-expect
		(e-method-eq (negated "false")
			(lhs
				(e-call (constraint-fn-var 633)
					(e-lookup-local
						(p-assign (ident "combined")))
					(e-string
						(e-literal (string "x")))
					(e-string
						(e-literal (string "b")))))
			(rhs
				(e-tag (name "Err")
					(args
						(e-tag (name "BadA")))))))
	(s-expect
		(e-method-eq (negated "false")
			(lhs
				(e-call (constraint-fn-var 679)
					(e-lookup-local
						(p-assign (ident "combined")))
					(e-string
						(e-literal (string "a")))
					(e-string
						(e-literal (string "x")))))
			(rhs
				(e-tag (name "Err")
					(args
						(e-tag (name "BadB")
							(args
								(e-string
									(e-literal (string "x")))))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str -> Try(I64, [BadA])"))
		(patt (type "Str -> Try(I64, [BadB(Str)])"))
		(patt (type "Str, Str -> Try(I64, [BadA, BadB(Str), ..])")))
	(expressions
		(expr (type "Str -> Try(I64, [BadA])"))
		(expr (type "Str -> Try(I64, [BadB(Str)])"))
		(expr (type "Str, Str -> Try(I64, [BadA, BadB(Str), ..])"))))
~~~

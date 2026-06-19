# META
~~~ini
description=Two ? operators with different Err types work in one top-level expect, since each Err type is unwrapped and discarded rather than unified with a return type
type=snippet
~~~
# SOURCE
~~~roc
parse_a : Str -> Try(I64, [BadA])
parse_a = |_s| Ok(1)

parse_b : Str -> Try(I64, [BadB(Str)])
parse_b = |_s| Ok(1)

expect parse_a("1")? == parse_b("1")?
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,NamedUnderscore,OpBar,UpperIdent,NoSpaceOpenRound,Int,CloseRound,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,NamedUnderscore,OpBar,UpperIdent,NoSpaceOpenRound,Int,CloseRound,
KwExpect,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,NoSpaceOpQuestion,OpEquals,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,NoSpaceOpQuestion,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
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
					(p-ident (raw "_s")))
				(e-apply
					(e-tag (raw "Ok"))
					(e-int (raw "1")))))
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
					(p-ident (raw "_s")))
				(e-apply
					(e-tag (raw "Ok"))
					(e-int (raw "1")))))
		(s-expect
			(e-binop (op "==")
				(e-question-suffix
					(e-apply
						(e-ident (raw "parse_a"))
						(e-string
							(e-string-part (raw "1")))))
				(e-question-suffix
					(e-apply
						(e-ident (raw "parse_b"))
						(e-string
							(e-string-part (raw "1")))))))))
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
				(p-assign (ident "_s")))
			(e-tag (name "Ok")
				(args
					(e-num (value "1")))))
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
				(p-assign (ident "_s")))
			(e-tag (name "Ok")
				(args
					(e-num (value "1")))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "I64") (builtin))
					(ty-tag-union
						(ty-tag-name (name "BadB")
							(ty-lookup (name "Str") (builtin))))))))
	(s-expect
		(e-method-eq (negated "false")
			(lhs
				(e-match
					(match
						(cond
							(e-call (constraint-fn-var 357)
								(e-lookup-local
									(p-assign (ident "parse_a")))
								(e-string
									(e-literal (string "1")))))
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
									(e-expect-err (snippet "parse_a("1")?")
										(e-lookup-local
											(p-assign (ident "#err"))))))))))
			(rhs
				(e-match
					(match
						(cond
							(e-call (constraint-fn-var 422)
								(e-lookup-local
									(p-assign (ident "parse_b")))
								(e-string
									(e-literal (string "1")))))
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
									(e-expect-err (snippet "parse_b("1")?")
										(e-lookup-local
											(p-assign (ident "#err")))))))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str -> Try(I64, [BadA])"))
		(patt (type "Str -> Try(I64, [BadB(Str)])")))
	(expressions
		(expr (type "Str -> Try(I64, [BadA])"))
		(expr (type "Str -> Try(I64, [BadB(Str)])"))))
~~~

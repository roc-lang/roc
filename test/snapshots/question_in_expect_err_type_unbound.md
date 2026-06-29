# META
~~~ini
description=? directly inside a top-level expect constrains its operand to Try with the Err type left unbound, with no return-style typing
type=snippet
~~~
# SOURCE
~~~roc
parse : Str -> Try(U64, [BadNumber])
parse = |s| {
	if s == "5" { Ok(5) } else { Err(BadNumber) }
}

expect parse("5")? == 5
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
KwIf,LowerIdent,OpEquals,StringStart,StringPart,StringEnd,OpenCurly,UpperIdent,NoSpaceOpenRound,Int,CloseRound,CloseCurly,KwElse,OpenCurly,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseCurly,
CloseCurly,
KwExpect,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,NoSpaceOpQuestion,OpEquals,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "parse")
			(ty-fn
				(ty (name "Str"))
				(ty-apply
					(ty (name "Try"))
					(ty (name "U64"))
					(ty-tag-union
						(tags
							(ty (name "BadNumber")))))))
		(s-decl
			(p-ident (raw "parse"))
			(e-lambda
				(args
					(p-ident (raw "s")))
				(e-block
					(statements
						(e-if-then-else
							(e-binop (op "==")
								(e-ident (raw "s"))
								(e-string
									(e-string-part (raw "5"))))
							(e-block
								(statements
									(e-apply
										(e-tag (raw "Ok"))
										(e-int (raw "5")))))
							(e-block
								(statements
									(e-apply
										(e-tag (raw "Err"))
										(e-tag (raw "BadNumber"))))))))))
		(s-expect
			(e-binop (op "==")
				(e-question-suffix
					(e-apply
						(e-ident (raw "parse"))
						(e-string
							(e-string-part (raw "5")))))
				(e-int (raw "5"))))))
~~~
# FORMATTED
~~~roc
parse : Str -> Try(U64, [BadNumber])
parse = |s| {
	if s == "5" {
		Ok(5)
	} else {
		Err(BadNumber)
	}
}

expect parse("5")? == 5
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "parse"))
		(e-lambda
			(args
				(p-assign (ident "s")))
			(e-block
				(e-if
					(if-branches
						(if-branch
							(e-method-eq (negated "false")
								(lhs
									(e-lookup-local
										(p-assign (ident "s"))))
								(rhs
									(e-string
										(e-literal (string "5")))))
							(e-block
								(e-tag (name "Ok")
									(args
										(e-num (value "5")))))))
					(if-else
						(e-block
							(e-tag (name "Err")
								(args
									(e-tag (name "BadNumber")))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "U64") (builtin))
					(ty-tag-union
						(ty-tag-name (name "BadNumber")))))))
	(s-expect
		(e-method-eq (negated "false")
			(lhs
				(e-match
					(match
						(cond
							(e-call (constraint-fn-var 270)
								(e-lookup-local
									(p-assign (ident "parse")))
								(e-string
									(e-literal (string "5")))))
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
									(e-expect-err (snippet "parse("5")?")
										(e-lookup-local
											(p-assign (ident "#err"))))))))))
			(rhs
				(e-num (value "5"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str -> Try(U64, [BadNumber])")))
	(expressions
		(expr (type "Str -> Try(U64, [BadNumber])"))))
~~~

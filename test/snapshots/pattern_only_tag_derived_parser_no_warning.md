# META
~~~ini
description=Tags of a value whose type comes from match patterns and whose values come from a derived parser are not reported
type=file
~~~
# SOURCE
~~~roc
describe = |input|
	match Json.parse(input) {
		Ok(Alpha) => "alpha"
		Ok(Beta) => "beta"
		Err(_) => "invalid"
	}

main! = |_args| {
	echo!(describe("\"Alpha\""))
	Ok({})
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,
KwMatch,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpenCurly,
UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,OpFatArrow,StringStart,StringPart,StringEnd,
UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,OpFatArrow,StringStart,StringPart,StringEnd,
UpperIdent,NoSpaceOpenRound,Underscore,CloseRound,OpFatArrow,StringStart,StringPart,StringEnd,
CloseCurly,
LowerIdent,OpAssign,OpBar,NamedUnderscore,OpBar,OpenCurly,
LowerIdent,NoSpaceOpenRound,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,CloseRound,
UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "describe"))
			(e-lambda
				(args
					(p-ident (raw "input")))
				(e-match
					(e-apply
						(e-ident (raw "Json.parse"))
						(e-ident (raw "input")))
					(branches
						(branch
							(p-tag (raw "Ok")
								(p-tag (raw "Alpha")))
							(e-string
								(e-string-part (raw "alpha"))))
						(branch
							(p-tag (raw "Ok")
								(p-tag (raw "Beta")))
							(e-string
								(e-string-part (raw "beta"))))
						(branch
							(p-tag (raw "Err")
								(p-underscore))
							(e-string
								(e-string-part (raw "invalid"))))))))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-ident (raw "_args")))
				(e-block
					(statements
						(e-apply
							(e-ident (raw "echo!"))
							(e-apply
								(e-ident (raw "describe"))
								(e-string
									(e-string-part (raw "\"Alpha\"")))))
						(e-apply
							(e-tag (raw "Ok"))
							(e-record))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "echo!"))
		(e-hosted-lambda (symbol "echo!")
			(args
				(p-assign (ident "_echo_arg"))))
		(annotation
			(ty-fn (effectful true)
				(ty-lookup (name "Str") (builtin))
				(ty-record))))
	(d-let
		(p-assign (ident "describe"))
		(e-lambda
			(args
				(p-assign (ident "input")))
			(e-match
				(match
					(cond
						(e-call (constraint-fn-var 300)
							(e-lookup-external
								(builtin))
							(e-lookup-local
								(p-assign (ident "input")))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-string
									(e-literal (string "alpha")))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-string
									(e-literal (string "beta")))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-string
									(e-literal (string "invalid"))))))))))
	(d-let
		(p-assign (ident "main!"))
		(e-lambda
			(args
				(p-assign (ident "_args")))
			(e-block
				(s-expr
					(e-call (constraint-fn-var 407)
						(e-lookup-local
							(p-assign (ident "echo!")))
						(e-call (constraint-fn-var 406)
							(e-lookup-local
								(p-assign (ident "describe")))
							(e-string
								(e-literal (string ""Alpha""))))))
				(e-tag (name "Ok")
					(args
						(e-empty_record)))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str => {}"))
		(patt (type "Str -> b where [b.from_quote : Str -> Try(b, [BadQuotedBytes(Str)])]"))
		(patt (type "_arg => [Ok({})]")))
	(expressions
		(expr (type "Str => {}"))
		(expr (type "Str -> b where [b.from_quote : Str -> Try(b, [BadQuotedBytes(Str)])]"))
		(expr (type "_arg => [Ok({})]"))))
~~~

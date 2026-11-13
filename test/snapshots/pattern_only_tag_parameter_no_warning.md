# META
~~~ini
description=Tags a parameter is matched against come from callers, so the match patterns are not reported
type=file
~~~
# SOURCE
~~~roc
describe = |shape| {
	name = match shape {
		Circle => "circle"
		_ => "other"
	}
	corners = match shape {
		Square => "4"
		_ => "0"
	}
	"${name} with ${corners} corners"
}

main! = |_args| {
	echo!(describe(Square))
	Ok({})
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,KwMatch,LowerIdent,OpenCurly,
UpperIdent,OpFatArrow,StringStart,StringPart,StringEnd,
Underscore,OpFatArrow,StringStart,StringPart,StringEnd,
CloseCurly,
LowerIdent,OpAssign,KwMatch,LowerIdent,OpenCurly,
UpperIdent,OpFatArrow,StringStart,StringPart,StringEnd,
Underscore,OpFatArrow,StringStart,StringPart,StringEnd,
CloseCurly,
StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,
CloseCurly,
LowerIdent,OpAssign,OpBar,NamedUnderscore,OpBar,OpenCurly,
LowerIdent,NoSpaceOpenRound,LowerIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseRound,
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
					(p-ident (raw "shape")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "name"))
							(e-match
								(e-ident (raw "shape"))
								(branches
									(branch
										(p-tag (raw "Circle"))
										(e-string
											(e-string-part (raw "circle"))))
									(branch
										(p-underscore)
										(e-string
											(e-string-part (raw "other")))))))
						(s-decl
							(p-ident (raw "corners"))
							(e-match
								(e-ident (raw "shape"))
								(branches
									(branch
										(p-tag (raw "Square"))
										(e-string
											(e-string-part (raw "4"))))
									(branch
										(p-underscore)
										(e-string
											(e-string-part (raw "0")))))))
						(e-string
							(e-string-part (raw ""))
							(e-ident (raw "name"))
							(e-string-part (raw " with "))
							(e-ident (raw "corners"))
							(e-string-part (raw " corners")))))))
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
								(e-tag (raw "Square"))))
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
				(p-assign (ident "shape")))
			(e-block
				(s-let
					(p-assign (ident "name"))
					(e-match
						(match
							(cond
								(e-lookup-local
									(p-assign (ident "shape"))))
							(branches
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag)))
									(value
										(e-string
											(e-literal (string "circle")))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-underscore)))
									(value
										(e-string
											(e-literal (string "other")))))))))
				(s-let
					(p-assign (ident "corners"))
					(e-match
						(match
							(cond
								(e-lookup-local
									(p-assign (ident "shape"))))
							(branches
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag)))
									(value
										(e-string
											(e-literal (string "4")))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-underscore)))
									(value
										(e-string
											(e-literal (string "0")))))))))
				(e-block
					(s-let
						(p-assign (ident "#interp_0"))
						(e-lookup-local
							(p-assign (ident "name"))))
					(s-let
						(p-assign (ident "#interp_1"))
						(e-lookup-local
							(p-assign (ident "corners"))))
					(e-interpolation (constraint-fn-var 340) (dispatcher-var 50)
						(first
							(e-literal (string "")))
						(parts
							(e-lookup-local
								(p-assign (ident "#interp_0")))
							(e-literal (string " with "))
							(e-lookup-local
								(p-assign (ident "#interp_1")))
							(e-literal (string " corners"))))))))
	(d-let
		(p-assign (ident "main!"))
		(e-lambda
			(args
				(p-assign (ident "_args")))
			(e-block
				(s-expr
					(e-call (constraint-fn-var 385)
						(e-lookup-local
							(p-assign (ident "echo!")))
						(e-call (constraint-fn-var 384)
							(e-lookup-local
								(p-assign (ident "describe")))
							(e-tag (name "Square")))))
				(e-tag (name "Ok")
					(args
						(e-empty_record)))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str => {}"))
		(patt (type "[Circle, Square, ..] -> a where [a.from_interpolation : List(Str) -> Try(Iter(item) -> a, [InvalidInterpolation(Str)]), item.from_quote : Str -> Try(item, [BadQuotedBytes(Str)])]"))
		(patt (type "_arg => [Ok({})]")))
	(expressions
		(expr (type "Str => {}"))
		(expr (type "[Circle, Square, ..] -> a where [a.from_interpolation : List(Str) -> Try(Iter(item) -> a, [InvalidInterpolation(Str)]), item.from_quote : Str -> Try(item, [BadQuotedBytes(Str)])]"))
		(expr (type "_arg => [Ok({})]"))))
~~~

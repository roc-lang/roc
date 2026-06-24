# META
~~~ini
description=Match expression with guard conditions using if clauses
type=snippet
~~~
# SOURCE
~~~roc
describe : I64 -> Str
describe = |value| match value {
    x if x > 0 => "positive: ${x.to_str()}"
    x if x < 0 => "negative: ${x.to_str()}"
    _ => "other"
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
LowerIdent,KwIf,LowerIdent,OpGreaterThan,Int,OpFatArrow,StringStart,StringPart,OpenStringInterpolation,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,CloseStringInterpolation,StringPart,StringEnd,
LowerIdent,KwIf,LowerIdent,OpLessThan,Int,OpFatArrow,StringStart,StringPart,OpenStringInterpolation,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,CloseStringInterpolation,StringPart,StringEnd,
Underscore,OpFatArrow,StringStart,StringPart,StringEnd,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "describe")
			(ty-fn
				(ty (name "I64"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "describe"))
			(e-lambda
				(args
					(p-ident (raw "value")))
				(e-match
					(e-ident (raw "value"))
					(branches
						(branch
							(p-ident (raw "x"))
							(guard
								(e-binop (op ">")
									(e-ident (raw "x"))
									(e-int (raw "0"))))
							(e-string
								(e-string-part (raw "positive: "))
								(e-method-call (method ".to_str")
									(receiver
										(e-ident (raw "x")))
									(args))
								(e-string-part (raw ""))))
						(branch
							(p-ident (raw "x"))
							(guard
								(e-binop (op "<")
									(e-ident (raw "x"))
									(e-int (raw "0"))))
							(e-string
								(e-string-part (raw "negative: "))
								(e-method-call (method ".to_str")
									(receiver
										(e-ident (raw "x")))
									(args))
								(e-string-part (raw ""))))
						(branch
							(p-underscore)
							(e-string
								(e-string-part (raw "other"))))))))))
~~~
# FORMATTED
~~~roc
describe : I64 -> Str
describe = |value| match value {
	x if x > 0 => "positive: ${x.to_str()}"
	x if x < 0 => "negative: ${x.to_str()}"
	_ => "other"
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "describe"))
		(e-lambda
			(args
				(p-assign (ident "value")))
			(e-match
				(match
					(cond
						(e-lookup-local
							(p-assign (ident "value"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-assign (ident "x"))))
							(value
								(e-block
									(s-let
										(p-assign (ident "#interp_0"))
										(e-dispatch-call (method "to_str") (constraint-fn-var 195)
											(receiver
												(e-lookup-local
													(p-assign (ident "x"))))
											(args)))
									(e-interpolation (constraint-fn-var 265)
										(first
											(e-literal (string "positive: ")))
										(parts
											(e-lookup-local
												(p-assign (ident "#interp_0")))
											(e-literal (string ""))))))
							(guard
								(e-dispatch-call (method "is_gt") (constraint-fn-var 106)
									(receiver
										(e-lookup-local
											(p-assign (ident "x"))))
									(args
										(e-num (value "0"))))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-assign (ident "x"))))
							(value
								(e-block
									(s-let
										(p-assign (ident "#interp_1"))
										(e-dispatch-call (method "to_str") (constraint-fn-var 392)
											(receiver
												(e-lookup-local
													(p-assign (ident "x"))))
											(args)))
									(e-interpolation (constraint-fn-var 462)
										(first
											(e-literal (string "negative: ")))
										(parts
											(e-lookup-local
												(p-assign (ident "#interp_1")))
											(e-literal (string ""))))))
							(guard
								(e-dispatch-call (method "is_lt") (constraint-fn-var 303)
									(receiver
										(e-lookup-local
											(p-assign (ident "x"))))
									(args
										(e-num (value "0"))))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-underscore)))
							(value
								(e-string
									(e-literal (string "other")))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "I64") (builtin))
				(ty-lookup (name "Str") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "I64 -> Str")))
	(expressions
		(expr (type "I64 -> Str"))))
~~~

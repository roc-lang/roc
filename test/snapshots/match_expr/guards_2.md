# META
~~~ini
description=Match expression with guard conditions on list patterns
type=snippet
~~~
# SOURCE
~~~roc
describe : List(I64) -> Str
describe = |value| match value {
    [first, .. as rest] if List.len(rest) > 5 => "long list starting with ${first.to_str()}"
    [x, y] if x == y => "pair of equal values: ${x.to_str()}"
    _ => "other"
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
OpenSquare,LowerIdent,Comma,DoubleDot,KwAs,LowerIdent,CloseSquare,KwIf,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpGreaterThan,Int,OpFatArrow,StringStart,StringPart,OpenStringInterpolation,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,CloseStringInterpolation,StringPart,StringEnd,
OpenSquare,LowerIdent,Comma,LowerIdent,CloseSquare,KwIf,LowerIdent,OpEquals,LowerIdent,OpFatArrow,StringStart,StringPart,OpenStringInterpolation,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,CloseStringInterpolation,StringPart,StringEnd,
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
				(ty-apply
					(ty (name "List"))
					(ty (name "I64")))
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
							(p-list
								(p-ident (raw "first"))
								(p-list-rest (name "rest")))
							(guard
								(e-binop (op ">")
									(e-apply
										(e-ident (raw "List.len"))
										(e-ident (raw "rest")))
									(e-int (raw "5"))))
							(e-string
								(e-string-part (raw "long list starting with "))
								(e-field-access
									(e-ident (raw "first"))
									(e-apply
										(e-ident (raw "to_str"))))
								(e-string-part (raw ""))))
						(branch
							(p-list
								(p-ident (raw "x"))
								(p-ident (raw "y")))
							(guard
								(e-binop (op "==")
									(e-ident (raw "x"))
									(e-ident (raw "y"))))
							(e-string
								(e-string-part (raw "pair of equal values: "))
								(e-field-access
									(e-ident (raw "x"))
									(e-apply
										(e-ident (raw "to_str"))))
								(e-string-part (raw ""))))
						(branch
							(p-underscore)
							(e-string
								(e-string-part (raw "other"))))))))))
~~~
# FORMATTED
~~~roc
describe : List(I64) -> Str
describe = |value| match value {
	[first, .. as rest] if List.len(rest) > 5 => "long list starting with ${first.to_str()}"
	[x, y] if x == y => "pair of equal values: ${x.to_str()}"
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
									(p-list
										(patterns
											(p-assign (ident "first")))
										(rest-at (index 1)
											(p-assign (ident "rest"))))))
							(value
								(e-string
									(e-literal (string "long list starting with "))
									(e-dot-access (field "to_str")
										(receiver
											(e-lookup-local
												(p-assign (ident "first"))))
										(args))
									(e-literal (string ""))))
							(guard
								(e-binop (op "gt")
									(e-call
										(e-lookup-external
											(builtin))
										(e-lookup-local
											(p-assign (ident "rest"))))
									(e-num (value "5")))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-list
										(patterns
											(p-assign (ident "x"))
											(p-assign (ident "y"))))))
							(value
								(e-string
									(e-literal (string "pair of equal values: "))
									(e-dot-access (field "to_str")
										(receiver
											(e-lookup-local
												(p-assign (ident "x"))))
										(args))
									(e-literal (string ""))))
							(guard
								(e-binop (op "eq")
									(e-lookup-local
										(p-assign (ident "x")))
									(e-lookup-local
										(p-assign (ident "y"))))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-underscore)))
							(value
								(e-string
									(e-literal (string "other")))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "List") (builtin)
					(ty-lookup (name "I64") (builtin)))
				(ty-lookup (name "Str") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "List(I64) -> Str")))
	(expressions
		(expr (type "List(I64) -> Str"))))
~~~

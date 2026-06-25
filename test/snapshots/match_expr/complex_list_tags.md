# META
~~~ini
description=Match expression with complex list patterns containing tagged values
type=expr
~~~
# SOURCE
~~~roc
match events {
    [] => "no events"
    [Click(x, y)] => "single click at (${Num.toStr(x)}, ${Num.toStr(y)})"
    [KeyPress(key), .. as rest] => "key ${key} pressed, ${Num.toStr(List.len(rest))} more events"
    [Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr(dx)},${Num.toStr(dy)} then ${Num.toStr(dx2)},${Num.toStr(dy2)}"
    [Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr(amount)} then click at ${Num.toStr(x)},${Num.toStr(y)}"
    _ => "other event pattern"
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
OpenSquare,CloseSquare,OpFatArrow,StringStart,StringPart,StringEnd,
OpenSquare,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,CloseSquare,OpFatArrow,StringStart,StringPart,OpenStringInterpolation,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseStringInterpolation,StringPart,OpenStringInterpolation,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseStringInterpolation,StringPart,StringEnd,
OpenSquare,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,DoubleDot,KwAs,LowerIdent,CloseSquare,OpFatArrow,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,OpenStringInterpolation,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,CloseStringInterpolation,StringPart,StringEnd,
OpenSquare,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,Comma,DoubleDot,KwAs,LowerIdent,CloseSquare,OpFatArrow,StringStart,StringPart,OpenStringInterpolation,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseStringInterpolation,StringPart,OpenStringInterpolation,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseStringInterpolation,StringPart,OpenStringInterpolation,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseStringInterpolation,StringPart,OpenStringInterpolation,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseStringInterpolation,StringPart,StringEnd,
OpenSquare,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,Comma,DoubleDot,KwAs,LowerIdent,CloseSquare,OpFatArrow,StringStart,StringPart,OpenStringInterpolation,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseStringInterpolation,StringPart,OpenStringInterpolation,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseStringInterpolation,StringPart,OpenStringInterpolation,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseStringInterpolation,StringPart,StringEnd,
Underscore,OpFatArrow,StringStart,StringPart,StringEnd,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "events"))
	(branches
		(branch
			(p-list)
			(e-string
				(e-string-part (raw "no events"))))
		(branch
			(p-list
				(p-tag (raw "Click")
					(p-ident (raw "x"))
					(p-ident (raw "y"))))
			(e-string
				(e-string-part (raw "single click at ("))
				(e-apply
					(e-ident (raw "Num.toStr"))
					(e-ident (raw "x")))
				(e-string-part (raw ", "))
				(e-apply
					(e-ident (raw "Num.toStr"))
					(e-ident (raw "y")))
				(e-string-part (raw ")"))))
		(branch
			(p-list
				(p-tag (raw "KeyPress")
					(p-ident (raw "key")))
				(p-list-rest (name "rest")))
			(e-string
				(e-string-part (raw "key "))
				(e-ident (raw "key"))
				(e-string-part (raw " pressed, "))
				(e-apply
					(e-ident (raw "Num.toStr"))
					(e-apply
						(e-ident (raw "List.len"))
						(e-ident (raw "rest"))))
				(e-string-part (raw " more events"))))
		(branch
			(p-list
				(p-tag (raw "Move")
					(p-ident (raw "dx"))
					(p-ident (raw "dy")))
				(p-tag (raw "Move")
					(p-ident (raw "dx2"))
					(p-ident (raw "dy2")))
				(p-list-rest (name "others")))
			(e-string
				(e-string-part (raw "moved "))
				(e-apply
					(e-ident (raw "Num.toStr"))
					(e-ident (raw "dx")))
				(e-string-part (raw ","))
				(e-apply
					(e-ident (raw "Num.toStr"))
					(e-ident (raw "dy")))
				(e-string-part (raw " then "))
				(e-apply
					(e-ident (raw "Num.toStr"))
					(e-ident (raw "dx2")))
				(e-string-part (raw ","))
				(e-apply
					(e-ident (raw "Num.toStr"))
					(e-ident (raw "dy2")))
				(e-string-part (raw ""))))
		(branch
			(p-list
				(p-tag (raw "Scroll")
					(p-ident (raw "amount")))
				(p-tag (raw "Click")
					(p-ident (raw "x"))
					(p-ident (raw "y")))
				(p-list-rest (name "remaining")))
			(e-string
				(e-string-part (raw "scroll "))
				(e-apply
					(e-ident (raw "Num.toStr"))
					(e-ident (raw "amount")))
				(e-string-part (raw " then click at "))
				(e-apply
					(e-ident (raw "Num.toStr"))
					(e-ident (raw "x")))
				(e-string-part (raw ","))
				(e-apply
					(e-ident (raw "Num.toStr"))
					(e-ident (raw "y")))
				(e-string-part (raw ""))))
		(branch
			(p-underscore)
			(e-string
				(e-string-part (raw "other event pattern"))))))
~~~
# FORMATTED
~~~roc
match events {
	[] => "no events"
	[Click(x, y)] => "single click at (${Num.toStr(x)}, ${Num.toStr(y)})"
	[KeyPress(key), .. as rest] => "key ${key} pressed, ${Num.toStr(List.len(rest))} more events"
	[Move(dx, dy), Move(dx2, dy2), .. as others] => "moved ${Num.toStr(dx)},${Num.toStr(dy)} then ${Num.toStr(dx2)},${Num.toStr(dy2)}"
	[Scroll(amount), Click(x, y), .. as remaining] => "scroll ${Num.toStr(amount)} then click at ${Num.toStr(x)},${Num.toStr(y)}"
	_ => "other event pattern"
}
~~~
# CANONICALIZE
~~~clojure
(e-match
	(match
		(cond
			(e-runtime-error (tag "ident_not_in_scope")))
		(branches
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns))))
				(value
					(e-string
						(e-literal (string "no events")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-applied-tag)))))
				(value
					(e-block
						(s-let
							(p-assign (ident "#interp_0"))
							(e-call
								(e-runtime-error (tag "qualified_ident_does_not_exist"))
								(e-lookup-local
									(p-assign (ident "x")))))
						(s-let
							(p-assign (ident "#interp_1"))
							(e-call
								(e-runtime-error (tag "qualified_ident_does_not_exist"))
								(e-lookup-local
									(p-assign (ident "y")))))
						(e-interpolation
							(first
								(e-literal (string "single click at (")))
							(parts
								(e-lookup-local
									(p-assign (ident "#interp_0")))
								(e-literal (string ", "))
								(e-lookup-local
									(p-assign (ident "#interp_1")))
								(e-literal (string ")")))))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-applied-tag))
							(rest-at (index 1)
								(p-assign (ident "rest"))))))
				(value
					(e-block
						(s-let
							(p-assign (ident "#interp_2"))
							(e-lookup-local
								(p-assign (ident "key"))))
						(s-let
							(p-assign (ident "#interp_3"))
							(e-call
								(e-runtime-error (tag "qualified_ident_does_not_exist"))
								(e-call
									(e-lookup-external
										(builtin))
									(e-lookup-local
										(p-assign (ident "rest"))))))
						(e-interpolation
							(first
								(e-literal (string "key ")))
							(parts
								(e-lookup-local
									(p-assign (ident "#interp_2")))
								(e-literal (string " pressed, "))
								(e-lookup-local
									(p-assign (ident "#interp_3")))
								(e-literal (string " more events")))))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-applied-tag)
								(p-applied-tag))
							(rest-at (index 2)
								(p-assign (ident "others"))))))
				(value
					(e-block
						(s-let
							(p-assign (ident "#interp_4"))
							(e-call
								(e-runtime-error (tag "qualified_ident_does_not_exist"))
								(e-lookup-local
									(p-assign (ident "dx")))))
						(s-let
							(p-assign (ident "#interp_5"))
							(e-call
								(e-runtime-error (tag "qualified_ident_does_not_exist"))
								(e-lookup-local
									(p-assign (ident "dy")))))
						(s-let
							(p-assign (ident "#interp_6"))
							(e-call
								(e-runtime-error (tag "qualified_ident_does_not_exist"))
								(e-lookup-local
									(p-assign (ident "dx2")))))
						(s-let
							(p-assign (ident "#interp_7"))
							(e-call
								(e-runtime-error (tag "qualified_ident_does_not_exist"))
								(e-lookup-local
									(p-assign (ident "dy2")))))
						(e-interpolation
							(first
								(e-literal (string "moved ")))
							(parts
								(e-lookup-local
									(p-assign (ident "#interp_4")))
								(e-literal (string ","))
								(e-lookup-local
									(p-assign (ident "#interp_5")))
								(e-literal (string " then "))
								(e-lookup-local
									(p-assign (ident "#interp_6")))
								(e-literal (string ","))
								(e-lookup-local
									(p-assign (ident "#interp_7")))
								(e-literal (string "")))))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-applied-tag)
								(p-applied-tag))
							(rest-at (index 2)
								(p-assign (ident "remaining"))))))
				(value
					(e-block
						(s-let
							(p-assign (ident "#interp_8"))
							(e-call
								(e-runtime-error (tag "qualified_ident_does_not_exist"))
								(e-lookup-local
									(p-assign (ident "amount")))))
						(s-let
							(p-assign (ident "#interp_9"))
							(e-call
								(e-runtime-error (tag "qualified_ident_does_not_exist"))
								(e-lookup-local
									(p-assign (ident "x")))))
						(s-let
							(p-assign (ident "#interp_10"))
							(e-call
								(e-runtime-error (tag "qualified_ident_does_not_exist"))
								(e-lookup-local
									(p-assign (ident "y")))))
						(e-interpolation
							(first
								(e-literal (string "scroll ")))
							(parts
								(e-lookup-local
									(p-assign (ident "#interp_8")))
								(e-literal (string " then click at "))
								(e-lookup-local
									(p-assign (ident "#interp_9")))
								(e-literal (string ","))
								(e-lookup-local
									(p-assign (ident "#interp_10")))
								(e-literal (string "")))))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-underscore)))
				(value
					(e-string
						(e-literal (string "other event pattern"))))))))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~

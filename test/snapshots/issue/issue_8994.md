# META
~~~ini
description=Issue 8994 - Generic function with List(a) -> List(a) should accept List(Str)
type=snippet
~~~
# SOURCE
~~~roc
duplicate : List(a) -> List(a)
duplicate = |l| {
    match l {
        [] => []
        [e, .. as rest] => {
            List.concat([e, e], duplicate(rest))
        }
    }
}

result = duplicate(["a", "b", "c"])
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
KwMatch,LowerIdent,OpenCurly,
OpenSquare,CloseSquare,OpFatArrow,OpenSquare,CloseSquare,
OpenSquare,LowerIdent,Comma,DoubleDot,KwAs,LowerIdent,CloseSquare,OpFatArrow,OpenCurly,
UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,OpenSquare,LowerIdent,Comma,LowerIdent,CloseSquare,Comma,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,
CloseCurly,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,OpenSquare,StringStart,StringPart,StringEnd,Comma,StringStart,StringPart,StringEnd,Comma,StringStart,StringPart,StringEnd,CloseSquare,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "duplicate")
			(ty-fn
				(ty-apply
					(ty (name "List"))
					(ty-var (raw "a")))
				(ty-apply
					(ty (name "List"))
					(ty-var (raw "a")))))
		(s-decl
			(p-ident (raw "duplicate"))
			(e-lambda
				(args
					(p-ident (raw "l")))
				(e-block
					(statements
						(e-match
							(e-ident (raw "l"))
							(branches
								(branch
									(p-list)
									(e-list))
								(branch
									(p-list
										(p-ident (raw "e"))
										(p-list-rest (name "rest")))
									(e-block
										(statements
											(e-apply
												(e-ident (raw "List.concat"))
												(e-list
													(e-ident (raw "e"))
													(e-ident (raw "e")))
												(e-apply
													(e-ident (raw "duplicate"))
													(e-ident (raw "rest")))))))))))))
		(s-decl
			(p-ident (raw "result"))
			(e-apply
				(e-ident (raw "duplicate"))
				(e-list
					(e-string
						(e-string-part (raw "a")))
					(e-string
						(e-string-part (raw "b")))
					(e-string
						(e-string-part (raw "c"))))))))
~~~
# FORMATTED
~~~roc
duplicate : List(a) -> List(a)
duplicate = |l| {
	match l {
		[] => []
		[e, .. as rest] => {
			List.concat([e, e], duplicate(rest))
		}
	}
}

result = duplicate(["a", "b", "c"])
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "duplicate"))
		(e-closure
			(captures
				(capture (ident "duplicate")))
			(e-lambda
				(args
					(p-assign (ident "l")))
				(e-block
					(e-match
						(match
							(cond
								(e-lookup-local
									(p-assign (ident "l"))))
							(branches
								(branch
									(patterns
										(pattern (degenerate false)
											(p-list
												(patterns))))
									(value
										(e-empty_list)))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-list
												(patterns
													(p-assign (ident "e")))
												(rest-at (index 1)
													(p-assign (ident "rest"))))))
									(value
										(e-block
											(e-call
												(e-lookup-external
													(builtin))
												(e-list
													(elems
														(e-lookup-local
															(p-assign (ident "e")))
														(e-lookup-local
															(p-assign (ident "e")))))
												(e-call
													(e-lookup-local
														(p-assign (ident "duplicate")))
													(e-lookup-local
														(p-assign (ident "rest"))))))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "List") (builtin)
					(ty-rigid-var (name "a")))
				(ty-apply (name "List") (builtin)
					(ty-rigid-var-lookup (ty-rigid-var (name "a")))))))
	(d-let
		(p-assign (ident "result"))
		(e-call
			(e-lookup-local
				(p-assign (ident "duplicate")))
			(e-list
				(elems
					(e-string
						(e-literal (string "a")))
					(e-string
						(e-literal (string "b")))
					(e-string
						(e-literal (string "c"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "List(a) -> List(a)"))
		(patt (type "List(Str)")))
	(expressions
		(expr (type "List(a) -> List(a)"))
		(expr (type "List(Str)"))))
~~~

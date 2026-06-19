# META
~~~ini
description=Match expression with nested patterns (tags containing records, lists with tags)
type=expr
~~~
# SOURCE
~~~roc
match data {
    Container({ items: [First(x), .. as rest] }) => x + List.len(rest)
    Container({ items: [] }) => 0
    Wrapper([Tag(value), Other(y)]) => value + y
    Simple(x) => x
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,OpenCurly,LowerIdent,OpColon,OpenSquare,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,DoubleDot,KwAs,LowerIdent,CloseSquare,CloseCurly,CloseRound,OpFatArrow,LowerIdent,OpPlus,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
UpperIdent,NoSpaceOpenRound,OpenCurly,LowerIdent,OpColon,OpenSquare,CloseSquare,CloseCurly,CloseRound,OpFatArrow,Int,
UpperIdent,NoSpaceOpenRound,OpenSquare,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseSquare,CloseRound,OpFatArrow,LowerIdent,OpPlus,LowerIdent,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "data"))
	(branches
		(branch
			(p-tag (raw "Container")
				(p-record
					(field (name "items") (rest false)
						(p-list
							(p-tag (raw "First")
								(p-ident (raw "x")))
							(p-list-rest (name "rest"))))))
			(e-binop (op "+")
				(e-ident (raw "x"))
				(e-apply
					(e-ident (raw "List.len"))
					(e-ident (raw "rest")))))
		(branch
			(p-tag (raw "Container")
				(p-record
					(field (name "items") (rest false)
						(p-list))))
			(e-int (raw "0")))
		(branch
			(p-tag (raw "Wrapper")
				(p-list
					(p-tag (raw "Tag")
						(p-ident (raw "value")))
					(p-tag (raw "Other")
						(p-ident (raw "y")))))
			(e-binop (op "+")
				(e-ident (raw "value"))
				(e-ident (raw "y"))))
		(branch
			(p-tag (raw "Simple")
				(p-ident (raw "x")))
			(e-ident (raw "x")))))
~~~
# FORMATTED
~~~roc
match data {
	Container({ items: [First(x), .. as rest] }) => x + List.len(rest)
	Container({ items: [] }) => 0
	Wrapper([Tag(value), Other(y)]) => value + y
	Simple(x) => x
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
						(p-applied-tag)))
				(value
					(e-dispatch-call (method "plus") (constraint-fn-var 68)
						(receiver
							(e-lookup-local
								(p-assign (ident "x"))))
						(args
							(e-call (constraint-fn-var 67)
								(e-lookup-external
									(builtin))
								(e-lookup-local
									(p-assign (ident "rest"))))))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-applied-tag)))
				(value
					(e-num (value "0"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-applied-tag)))
				(value
					(e-dispatch-call (method "plus") (constraint-fn-var 116)
						(receiver
							(e-lookup-local
								(p-assign (ident "value"))))
						(args
							(e-lookup-local
								(p-assign (ident "y")))))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-applied-tag)))
				(value
					(e-lookup-local
						(p-assign (ident "x"))))))))
~~~
# TYPES
~~~clojure
(expr (type "U64"))
~~~

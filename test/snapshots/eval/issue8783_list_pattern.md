# META
~~~ini
description=Regression test for issue 8783: match on list element from list pattern
type=snippet
~~~
# SOURCE
~~~roc
Elem := [
	Element(Str, List(Elem)),
	Text(Str),
]

elem : Elem
elem = Element("div", [Text("hello")])

children : List(Elem)
children = match elem {
	Element(_tag, c) => c
	Text(_) => []
}

result = match children {
	[child, ..] => match child {
		Text(_) => "text"
		Element(_, _) => "element"
	}
	[] => "empty"
}

expect result == "text"
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,
UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseRound,Comma,
UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,Comma,
CloseSquare,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,Comma,OpenSquare,UpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,CloseSquare,CloseRound,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpAssign,KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,NamedUnderscore,Comma,LowerIdent,CloseRound,OpFatArrow,LowerIdent,
UpperIdent,NoSpaceOpenRound,Underscore,CloseRound,OpFatArrow,OpenSquare,CloseSquare,
CloseCurly,
LowerIdent,OpAssign,KwMatch,LowerIdent,OpenCurly,
OpenSquare,LowerIdent,Comma,DoubleDot,CloseSquare,OpFatArrow,KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,Underscore,CloseRound,OpFatArrow,StringStart,StringPart,StringEnd,
UpperIdent,NoSpaceOpenRound,Underscore,Comma,Underscore,CloseRound,OpFatArrow,StringStart,StringPart,StringEnd,
CloseCurly,
OpenSquare,CloseSquare,OpFatArrow,StringStart,StringPart,StringEnd,
CloseCurly,
KwExpect,LowerIdent,OpEquals,StringStart,StringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Elem")
				(args))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "Element"))
						(ty (name "Str"))
						(ty-apply
							(ty (name "List"))
							(ty (name "Elem"))))
					(ty-apply
						(ty (name "Text"))
						(ty (name "Str"))))))
		(s-type-anno (name "elem")
			(ty (name "Elem")))
		(s-decl
			(p-ident (raw "elem"))
			(e-apply
				(e-tag (raw "Element"))
				(e-string
					(e-string-part (raw "div")))
				(e-list
					(e-apply
						(e-tag (raw "Text"))
						(e-string
							(e-string-part (raw "hello")))))))
		(s-type-anno (name "children")
			(ty-apply
				(ty (name "List"))
				(ty (name "Elem"))))
		(s-decl
			(p-ident (raw "children"))
			(e-match
				(e-ident (raw "elem"))
				(branches
					(branch
						(p-tag (raw "Element")
							(p-ident (raw "_tag"))
							(p-ident (raw "c")))
						(e-ident (raw "c")))
					(branch
						(p-tag (raw "Text")
							(p-underscore))
						(e-list)))))
		(s-decl
			(p-ident (raw "result"))
			(e-match
				(e-ident (raw "children"))
				(branches
					(branch
						(p-list
							(p-ident (raw "child"))
							(p-list-rest))
						(e-match
							(e-ident (raw "child"))
							(branches
								(branch
									(p-tag (raw "Text")
										(p-underscore))
									(e-string
										(e-string-part (raw "text"))))
								(branch
									(p-tag (raw "Element")
										(p-underscore)
										(p-underscore))
									(e-string
										(e-string-part (raw "element")))))))
					(branch
						(p-list)
						(e-string
							(e-string-part (raw "empty")))))))
		(s-expect
			(e-binop (op "==")
				(e-ident (raw "result"))
				(e-string
					(e-string-part (raw "text")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "elem"))
		(e-tag (name "Element")
			(args
				(e-string
					(e-literal (string "div")))
				(e-list
					(elems
						(e-tag (name "Text")
							(args
								(e-string
									(e-literal (string "hello")))))))))
		(annotation
			(ty-lookup (name "Elem") (local))))
	(d-let
		(p-assign (ident "children"))
		(e-match
			(match
				(cond
					(e-lookup-local
						(p-assign (ident "elem"))))
				(branches
					(branch
						(patterns
							(pattern (degenerate false)
								(p-applied-tag)))
						(value
							(e-lookup-local
								(p-assign (ident "c")))))
					(branch
						(patterns
							(pattern (degenerate false)
								(p-applied-tag)))
						(value
							(e-empty_list))))))
		(annotation
			(ty-apply (name "List") (builtin)
				(ty-lookup (name "Elem") (local)))))
	(d-let
		(p-assign (ident "result"))
		(e-match
			(match
				(cond
					(e-lookup-local
						(p-assign (ident "children"))))
				(branches
					(branch
						(patterns
							(pattern (degenerate false)
								(p-list
									(patterns
										(p-assign (ident "child")))
									(rest-at (index 1)))))
						(value
							(e-match
								(match
									(cond
										(e-lookup-local
											(p-assign (ident "child"))))
									(branches
										(branch
											(patterns
												(pattern (degenerate false)
													(p-applied-tag)))
											(value
												(e-string
													(e-literal (string "text")))))
										(branch
											(patterns
												(pattern (degenerate false)
													(p-applied-tag)))
											(value
												(e-string
													(e-literal (string "element"))))))))))
					(branch
						(patterns
							(pattern (degenerate false)
								(p-list
									(patterns))))
						(value
							(e-string
								(e-literal (string "empty")))))))))
	(s-nominal-decl
		(ty-header (name "Elem"))
		(ty-tag-union
			(ty-tag-name (name "Element")
				(ty-lookup (name "Str") (builtin))
				(ty-apply (name "List") (builtin)
					(ty-lookup (name "Elem") (local))))
			(ty-tag-name (name "Text")
				(ty-lookup (name "Str") (builtin)))))
	(s-expect
		(e-binop (op "eq")
			(e-lookup-local
				(p-assign (ident "result")))
			(e-string
				(e-literal (string "text"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Elem"))
		(patt (type "List(Elem)"))
		(patt (type "Str")))
	(type_decls
		(nominal (type "Elem")
			(ty-header (name "Elem"))))
	(expressions
		(expr (type "Elem"))
		(expr (type "List(Elem)"))
		(expr (type "Str"))))
~~~

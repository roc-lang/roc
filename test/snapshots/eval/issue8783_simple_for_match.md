# META
~~~ini
description=Regression test for issue 8783: List.fold with match on recursive opaque type elements
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

count_child : I64, Elem -> I64
count_child = |acc, child|
	match child {
		Text(_) => acc + 1
		Element(_, _) => acc + 10
	}

count : I64
count = List.fold(children, 0, count_child)

expect count == 1
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
LowerIdent,OpColon,UpperIdent,Comma,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,
KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,Underscore,CloseRound,OpFatArrow,LowerIdent,OpPlus,Int,
UpperIdent,NoSpaceOpenRound,Underscore,Comma,Underscore,CloseRound,OpFatArrow,LowerIdent,OpPlus,Int,
CloseCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,Comma,Int,Comma,LowerIdent,CloseRound,
KwExpect,LowerIdent,OpEquals,Int,
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
		(s-type-anno (name "count_child")
			(ty-fn
				(ty (name "I64"))
				(ty (name "Elem"))
				(ty (name "I64"))))
		(s-decl
			(p-ident (raw "count_child"))
			(e-lambda
				(args
					(p-ident (raw "acc"))
					(p-ident (raw "child")))
				(e-match
					(e-ident (raw "child"))
					(branches
						(branch
							(p-tag (raw "Text")
								(p-underscore))
							(e-binop (op "+")
								(e-ident (raw "acc"))
								(e-int (raw "1"))))
						(branch
							(p-tag (raw "Element")
								(p-underscore)
								(p-underscore))
							(e-binop (op "+")
								(e-ident (raw "acc"))
								(e-int (raw "10"))))))))
		(s-type-anno (name "count")
			(ty (name "I64")))
		(s-decl
			(p-ident (raw "count"))
			(e-apply
				(e-ident (raw "List.fold"))
				(e-ident (raw "children"))
				(e-int (raw "0"))
				(e-ident (raw "count_child"))))
		(s-expect
			(e-binop (op "==")
				(e-ident (raw "count"))
				(e-int (raw "1"))))))
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
		(p-assign (ident "count_child"))
		(e-lambda
			(args
				(p-assign (ident "acc"))
				(p-assign (ident "child")))
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
								(e-binop (op "add")
									(e-lookup-local
										(p-assign (ident "acc")))
									(e-num (value "1")))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-binop (op "add")
									(e-lookup-local
										(p-assign (ident "acc")))
									(e-num (value "10")))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "I64") (builtin))
				(ty-lookup (name "Elem") (local))
				(ty-lookup (name "I64") (builtin)))))
	(d-let
		(p-assign (ident "count"))
		(e-call
			(e-lookup-external
				(builtin))
			(e-lookup-local
				(p-assign (ident "children")))
			(e-num (value "0"))
			(e-lookup-local
				(p-assign (ident "count_child"))))
		(annotation
			(ty-lookup (name "I64") (builtin))))
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
				(p-assign (ident "count")))
			(e-num (value "1")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Elem"))
		(patt (type "List(Elem)"))
		(patt (type "I64, Elem -> I64"))
		(patt (type "I64")))
	(type_decls
		(nominal (type "Elem")
			(ty-header (name "Elem"))))
	(expressions
		(expr (type "Elem"))
		(expr (type "List(Elem)"))
		(expr (type "I64, Elem -> I64"))
		(expr (type "I64"))))
~~~

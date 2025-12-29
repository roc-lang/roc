# META
~~~ini
description=Regression test for issue 8783: List.fold with recursive callback on opaque types
type=snippet
~~~
# SOURCE
~~~roc
Elem := [
	Element(Str, List(Elem)),
	Text(Str),
]

process_child : Str, Elem -> Str
process_child = |acc, child|
	"${acc} ${process(child)}"

process : Elem -> Str
process = |elem|
	match elem {
		Element(tag, children) =>
			"${tag}:${List.fold(children, "", process_child)}"
		Text(content) => content
	}

root : Elem
root = Element("div", [Text("hello")])

result : Str
result = process(root)

expect result == "div: hello"
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
LowerIdent,OpColon,UpperIdent,Comma,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,
StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,OpenStringInterpolation,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseStringInterpolation,StringPart,StringEnd,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,
KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpFatArrow,
StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,OpenStringInterpolation,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,Comma,StringStart,StringPart,StringEnd,Comma,LowerIdent,CloseRound,CloseStringInterpolation,StringPart,StringEnd,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,
CloseCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,Comma,OpenSquare,UpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,CloseSquare,CloseRound,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
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
		(s-type-anno (name "process_child")
			(ty-fn
				(ty (name "Str"))
				(ty (name "Elem"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "process_child"))
			(e-lambda
				(args
					(p-ident (raw "acc"))
					(p-ident (raw "child")))
				(e-string
					(e-string-part (raw ""))
					(e-ident (raw "acc"))
					(e-string-part (raw " "))
					(e-apply
						(e-ident (raw "process"))
						(e-ident (raw "child")))
					(e-string-part (raw "")))))
		(s-type-anno (name "process")
			(ty-fn
				(ty (name "Elem"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "process"))
			(e-lambda
				(args
					(p-ident (raw "elem")))
				(e-match
					(e-ident (raw "elem"))
					(branches
						(branch
							(p-tag (raw "Element")
								(p-ident (raw "tag"))
								(p-ident (raw "children")))
							(e-string
								(e-string-part (raw ""))
								(e-ident (raw "tag"))
								(e-string-part (raw ":"))
								(e-apply
									(e-ident (raw "List.fold"))
									(e-ident (raw "children"))
									(e-string
										(e-string-part (raw "")))
									(e-ident (raw "process_child")))
								(e-string-part (raw ""))))
						(branch
							(p-tag (raw "Text")
								(p-ident (raw "content")))
							(e-ident (raw "content")))))))
		(s-type-anno (name "root")
			(ty (name "Elem")))
		(s-decl
			(p-ident (raw "root"))
			(e-apply
				(e-tag (raw "Element"))
				(e-string
					(e-string-part (raw "div")))
				(e-list
					(e-apply
						(e-tag (raw "Text"))
						(e-string
							(e-string-part (raw "hello")))))))
		(s-type-anno (name "result")
			(ty (name "Str")))
		(s-decl
			(p-ident (raw "result"))
			(e-apply
				(e-ident (raw "process"))
				(e-ident (raw "root"))))
		(s-expect
			(e-binop (op "==")
				(e-ident (raw "result"))
				(e-string
					(e-string-part (raw "div: hello")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "process_child"))
		(e-closure
			(captures
				(capture (ident "process")))
			(e-lambda
				(args
					(p-assign (ident "acc"))
					(p-assign (ident "child")))
				(e-string
					(e-literal (string ""))
					(e-lookup-local
						(p-assign (ident "acc")))
					(e-literal (string " "))
					(e-call
						(e-lookup-local
							(p-assign (ident "process")))
						(e-lookup-local
							(p-assign (ident "child"))))
					(e-literal (string "")))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-lookup (name "Elem") (local))
				(ty-lookup (name "Str") (builtin)))))
	(d-let
		(p-assign (ident "process"))
		(e-closure
			(captures
				(capture (ident "process_child")))
			(e-lambda
				(args
					(p-assign (ident "elem")))
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
									(e-string
										(e-literal (string ""))
										(e-lookup-local
											(p-assign (ident "tag")))
										(e-literal (string ":"))
										(e-call
											(e-lookup-external
												(builtin))
											(e-lookup-local
												(p-assign (ident "children")))
											(e-string
												(e-literal (string "")))
											(e-lookup-local
												(p-assign (ident "process_child"))))
										(e-literal (string "")))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag)))
								(value
									(e-lookup-local
										(p-assign (ident "content"))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Elem") (local))
				(ty-lookup (name "Str") (builtin)))))
	(d-let
		(p-assign (ident "root"))
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
		(p-assign (ident "result"))
		(e-call
			(e-lookup-local
				(p-assign (ident "process")))
			(e-lookup-local
				(p-assign (ident "root"))))
		(annotation
			(ty-lookup (name "Str") (builtin))))
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
				(e-literal (string "div: hello"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str, Elem -> Str"))
		(patt (type "Elem -> Str"))
		(patt (type "Elem"))
		(patt (type "Str")))
	(type_decls
		(nominal (type "Elem")
			(ty-header (name "Elem"))))
	(expressions
		(expr (type "Str, Elem -> Str"))
		(expr (type "Elem -> Str"))
		(expr (type "Elem"))
		(expr (type "Str"))))
~~~

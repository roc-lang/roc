# META
~~~ini
description=Regression test for issue 8773: pattern matching on polymorphic function results with opaque types
type=snippet
~~~
# SOURCE
~~~roc
Item := [
	Element(Str, List(Item)),
	Text(Str),
]

get_text : Item -> Str
get_text = |item|
	match item {
		Element(tag, _) => tag
		Text(content) => content
	}

items : List(Item)
items = [Text("hello"), Text("world")]

result : Str
result = match List.first(items) {
	Ok(item) => get_text(item)
	Err(_) => "empty"
}

expect result == "hello"
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
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,
KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,Underscore,CloseRound,OpFatArrow,LowerIdent,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,
CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpAssign,OpenSquare,UpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,CloseSquare,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,KwMatch,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpenCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
UpperIdent,NoSpaceOpenRound,Underscore,CloseRound,OpFatArrow,StringStart,StringPart,StringEnd,
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
			(header (name "Item")
				(args))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "Element"))
						(ty (name "Str"))
						(ty-apply
							(ty (name "List"))
							(ty (name "Item"))))
					(ty-apply
						(ty (name "Text"))
						(ty (name "Str"))))))
		(s-type-anno (name "get_text")
			(ty-fn
				(ty (name "Item"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "get_text"))
			(e-lambda
				(args
					(p-ident (raw "item")))
				(e-match
					(e-ident (raw "item"))
					(branches
						(branch
							(p-tag (raw "Element")
								(p-ident (raw "tag"))
								(p-underscore))
							(e-ident (raw "tag")))
						(branch
							(p-tag (raw "Text")
								(p-ident (raw "content")))
							(e-ident (raw "content")))))))
		(s-type-anno (name "items")
			(ty-apply
				(ty (name "List"))
				(ty (name "Item"))))
		(s-decl
			(p-ident (raw "items"))
			(e-list
				(e-apply
					(e-tag (raw "Text"))
					(e-string
						(e-string-part (raw "hello"))))
				(e-apply
					(e-tag (raw "Text"))
					(e-string
						(e-string-part (raw "world"))))))
		(s-type-anno (name "result")
			(ty (name "Str")))
		(s-decl
			(p-ident (raw "result"))
			(e-match
				(e-apply
					(e-ident (raw "List.first"))
					(e-ident (raw "items")))
				(branches
					(branch
						(p-tag (raw "Ok")
							(p-ident (raw "item")))
						(e-apply
							(e-ident (raw "get_text"))
							(e-ident (raw "item"))))
					(branch
						(p-tag (raw "Err")
							(p-underscore))
						(e-string
							(e-string-part (raw "empty")))))))
		(s-expect
			(e-binop (op "==")
				(e-ident (raw "result"))
				(e-string
					(e-string-part (raw "hello")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "get_text"))
		(e-lambda
			(args
				(p-assign (ident "item")))
			(e-match
				(match
					(cond
						(e-lookup-local
							(p-assign (ident "item"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-lookup-local
									(p-assign (ident "tag")))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-lookup-local
									(p-assign (ident "content")))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Item") (local))
				(ty-lookup (name "Str") (builtin)))))
	(d-let
		(p-assign (ident "items"))
		(e-list
			(elems
				(e-tag (name "Text")
					(args
						(e-string
							(e-literal (string "hello")))))
				(e-tag (name "Text")
					(args
						(e-string
							(e-literal (string "world")))))))
		(annotation
			(ty-apply (name "List") (builtin)
				(ty-lookup (name "Item") (local)))))
	(d-let
		(p-assign (ident "result"))
		(e-match
			(match
				(cond
					(e-call
						(e-lookup-external
							(builtin))
						(e-lookup-local
							(p-assign (ident "items")))))
				(branches
					(branch
						(patterns
							(pattern (degenerate false)
								(p-applied-tag)))
						(value
							(e-call
								(e-lookup-local
									(p-assign (ident "get_text")))
								(e-lookup-local
									(p-assign (ident "item"))))))
					(branch
						(patterns
							(pattern (degenerate false)
								(p-applied-tag)))
						(value
							(e-string
								(e-literal (string "empty"))))))))
		(annotation
			(ty-lookup (name "Str") (builtin))))
	(s-nominal-decl
		(ty-header (name "Item"))
		(ty-tag-union
			(ty-tag-name (name "Element")
				(ty-lookup (name "Str") (builtin))
				(ty-apply (name "List") (builtin)
					(ty-lookup (name "Item") (local))))
			(ty-tag-name (name "Text")
				(ty-lookup (name "Str") (builtin)))))
	(s-expect
		(e-binop (op "eq")
			(e-lookup-local
				(p-assign (ident "result")))
			(e-string
				(e-literal (string "hello"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Item -> Str"))
		(patt (type "List(Item)"))
		(patt (type "Str")))
	(type_decls
		(nominal (type "Item")
			(ty-header (name "Item"))))
	(expressions
		(expr (type "Item -> Str"))
		(expr (type "List(Item)"))
		(expr (type "Str"))))
~~~

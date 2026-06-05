# META
~~~ini
description=Type variable import runtime resolution
type=file
~~~
# SOURCE
~~~roc
join_all : List(item), item -> item
    where [item.join_with : List(item), item -> item]
join_all = |items, joiner| {
    import item as Item
    Item.join_with(items, joiner)
}

main! = |_| {
    result = join_all(["hello", "world"], ", ")
    result
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,LowerIdent,OpArrow,LowerIdent,
KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,LowerIdent,OpArrow,LowerIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,
KwImport,LowerIdent,KwAs,UpperIdent,
UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
CloseCurly,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,OpenSquare,StringStart,StringPart,StringEnd,Comma,StringStart,StringPart,StringEnd,CloseSquare,Comma,StringStart,StringPart,StringEnd,CloseRound,
LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "join_all")
			(ty-fn
				(ty-apply
					(ty (name "List"))
					(ty-var (raw "item")))
				(ty-var (raw "item"))
				(ty-var (raw "item")))
			(where
				(method (module-of "item") (name "join_with")
					(args
						(ty-apply
							(ty (name "List"))
							(ty-var (raw "item")))
						(ty-var (raw "item")))
					(ty-var (raw "item")))))
		(s-decl
			(p-ident (raw "join_all"))
			(e-lambda
				(args
					(p-ident (raw "items"))
					(p-ident (raw "joiner")))
				(e-block
					(statements
						(s-type-var-import (type_var "item") (alias "Item"))
						(e-apply
							(e-ident (raw "Item.join_with"))
							(e-ident (raw "items"))
							(e-ident (raw "joiner")))))))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "result"))
							(e-apply
								(e-ident (raw "join_all"))
								(e-list
									(e-string
										(e-string-part (raw "hello")))
									(e-string
										(e-string-part (raw "world"))))
								(e-string
									(e-string-part (raw ", ")))))
						(e-ident (raw "result"))))))))
~~~
# FORMATTED
~~~roc
join_all : List(item), item -> item
	where [item.join_with : List(item), item -> item]
join_all = |items, joiner| {
	import item as Item
	Item.join_with(items, joiner)
}

main! = |_| {
	result = join_all(["hello", "world"], ", ")
	result
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "join_all"))
		(e-lambda
			(args
				(p-assign (ident "items"))
				(p-assign (ident "joiner")))
			(e-block
				(e-call
					(e-lookup-type-var-member
						(member "join_with")
						(alias "Item"))
					(e-lookup-local
						(p-assign (ident "items")))
					(e-lookup-local
						(p-assign (ident "joiner"))))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "List") (builtin)
					(ty-rigid-var (name "item")))
				(ty-rigid-var-lookup (ty-rigid-var (name "item")))
				(ty-rigid-var-lookup (ty-rigid-var (name "item"))))
			(where
				(method (ty-rigid-var-lookup (ty-rigid-var (name "item"))) (name "join_with")
					(args
						(ty-apply (name "List") (builtin)
							(ty-rigid-var-lookup (ty-rigid-var (name "item"))))
						(ty-rigid-var-lookup (ty-rigid-var (name "item"))))
					(ty-rigid-var-lookup (ty-rigid-var (name "item")))))))
	(d-let
		(p-assign (ident "main!"))
		(e-closure
			(captures
				(capture (ident "join_all")))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(s-let
						(p-assign (ident "result"))
						(e-call
							(e-lookup-local
								(p-assign (ident "join_all")))
							(e-list
								(elems
									(e-string
										(e-literal (string "hello")))
									(e-string
										(e-literal (string "world")))))
							(e-string
								(e-literal (string ", ")))))
					(e-lookup-local
						(p-assign (ident "result"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "List(item), item -> item where [item.join_with : List(item), item -> item]"))
		(patt (type "_arg -> Str")))
	(expressions
		(expr (type "List(item), item -> item where [item.join_with : List(item), item -> item]"))
		(expr (type "_arg -> Str"))))
~~~

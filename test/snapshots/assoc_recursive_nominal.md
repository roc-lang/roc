# META
~~~ini
description=A genuinely recursive nominal type declared inside an associated block stays legal
type=file:Outer.roc
~~~
# SOURCE
~~~roc
Outer := [].{
    Rec := [Cons(U64, Rec), Nil]
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,UpperIdent,CloseRound,Comma,UpperIdent,CloseSquare,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Outer")
				(args))
			(ty-tag-union
				(tags))
			(associated
				(s-type-decl
					(header (name "Rec")
						(args))
					(ty-tag-union
						(tags
							(ty-apply
								(ty (name "Cons"))
								(ty (name "U64"))
								(ty (name "Rec")))
							(ty (name "Nil")))))))))
~~~
# FORMATTED
~~~roc
Outer := [].{
	Rec := [Cons(U64, Rec), Nil]
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-nominal-decl
		(ty-header (name "Outer"))
		(ty-tag-union))
	(s-nominal-decl
		(ty-header (name "Outer.Rec"))
		(ty-tag-union
			(ty-tag-name (name "Cons")
				(ty-lookup (name "U64") (builtin))
				(ty-lookup (name "Rec") (local)))
			(ty-tag-name (name "Nil")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(nominal (type "Outer")
			(ty-header (name "Outer")))
		(nominal (type "Outer.Rec")
			(ty-header (name "Outer.Rec"))))
	(expressions))
~~~

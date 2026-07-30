# META
~~~ini
description=Nominal type mod whose field depends on an unqualified nested associated type declared in the same type mod. This covers issue #9486's associated-definition arrangement.
type=file:ModType.roc
~~~
# SOURCE
~~~roc
ModType := {
    field : InternalType,
}.{
    InternalType := [Some, Other]
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenCurly,
LowerIdent,OpColon,UpperIdent,Comma,
CloseCurly,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "ModType")
				(args))
			(ty-record
				(anno-record-field (name "field")
					(ty (name "InternalType"))))
			(associated
				(s-type-decl
					(header (name "InternalType")
						(args))
					(ty-tag-union
						(tags
							(ty (name "Some"))
							(ty (name "Other")))))))))
~~~
# FORMATTED
~~~roc
ModType := {
	field : InternalType,
}.{
	InternalType := [Some, Other]
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-nominal-decl
		(ty-header (name "ModType"))
		(ty-record
			(field (field "field")
				(ty-lookup (name "InternalType") (local)))))
	(s-nominal-decl
		(ty-header (name "ModType.InternalType"))
		(ty-tag-union
			(ty-tag-name (name "Some"))
			(ty-tag-name (name "Other")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(nominal (type "ModType")
			(ty-header (name "ModType")))
		(nominal (type "ModType.InternalType")
			(ty-header (name "ModType.InternalType"))))
	(expressions))
~~~

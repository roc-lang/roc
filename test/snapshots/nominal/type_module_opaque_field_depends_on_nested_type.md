# META
~~~ini
description=Opaque type mod whose field depends on a nested associated type (qualified ModType.InternalType). This compiles because the nested type is exposed as ModType.InternalType.
type=file:ModType.roc
~~~
# SOURCE
~~~roc
ModType :: {
    field : ModType.InternalType,
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
UpperIdent,OpDoubleColon,OpenCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,Comma,
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
					(ty (name "ModType.InternalType"))))
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
ModType :: {
	field : ModType.InternalType,
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
				(ty-lookup (name "ModType.InternalType") (local)))))
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

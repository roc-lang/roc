# META
~~~ini
description=Opaque type module whose field depends on a nested associated type (qualified ModuleType.InternalType). This compiles because the nested type is exposed as ModuleType.InternalType.
type=file:ModuleType.roc
~~~
# SOURCE
~~~roc
ModuleType :: {
    field : ModuleType.InternalType,
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
	(type-module)
	(statements
		(s-type-decl
			(header (name "ModuleType")
				(args))
			(ty-record
				(anno-record-field (name "field")
					(ty (name "ModuleType.InternalType"))))
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
ModuleType :: {
	field : ModuleType.InternalType,
}.{
	InternalType := [Some, Other]
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-nominal-decl
		(ty-header (name "ModuleType"))
		(ty-record
			(field (field "field")
				(ty-lookup (name "ModuleType.InternalType") (local)))))
	(s-nominal-decl
		(ty-header (name "ModuleType.InternalType"))
		(ty-tag-union
			(ty-tag-name (name "Some"))
			(ty-tag-name (name "Other")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(nominal (type "ModuleType")
			(ty-header (name "ModuleType")))
		(nominal (type "ModuleType.InternalType")
			(ty-header (name "ModuleType.InternalType"))))
	(expressions))
~~~

# META
~~~ini
description=Nominal type mod whose field depends on a top-level type ALIAS. No warning expected: aliases are structurally transparent, so other mods can still see the field's structure.
type=file:ModType.roc
~~~
# SOURCE
~~~roc
InternalType : [Some, Other]

ModType := {
    field : InternalType,
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColon,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,
UpperIdent,OpColonEqual,OpenCurly,
LowerIdent,OpColon,UpperIdent,Comma,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "InternalType")
				(args))
			(ty-tag-union
				(tags
					(ty (name "Some"))
					(ty (name "Other")))))
		(s-type-decl
			(header (name "ModType")
				(args))
			(ty-record
				(anno-record-field (name "field")
					(ty (name "InternalType")))))))
~~~
# FORMATTED
~~~roc
InternalType : [Some, Other]

ModType := {
	field : InternalType,
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl
		(ty-header (name "InternalType"))
		(ty-tag-union
			(ty-tag-name (name "Some"))
			(ty-tag-name (name "Other"))))
	(s-nominal-decl
		(ty-header (name "ModType"))
		(ty-record
			(field (field "field")
				(ty-lookup (name "InternalType") (local))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias (type "InternalType")
			(ty-header (name "InternalType")))
		(nominal (type "ModType")
			(ty-header (name "ModType"))))
	(expressions))
~~~

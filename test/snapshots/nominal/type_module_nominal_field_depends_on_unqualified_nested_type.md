# META
~~~ini
description=Nominal type module whose field depends on an unqualified nested associated type declared in the same type module. This covers issue #9486's associated-definition arrangement.
type=file:ModuleType.roc
~~~
# SOURCE
~~~roc
ModuleType := {
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
	(type-module)
	(statements
		(s-type-decl
			(header (name "ModuleType")
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
ModuleType := {
	field : InternalType,
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
				(ty-lookup (name "InternalType") (local)))))
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

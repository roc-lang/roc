# META
~~~ini
description=Nominal type module whose field depends on a nested associated type (qualified ModuleType.InternalType). This should compile, since the nested type is exposed as ModuleType.InternalType. It currently reproduces a MISSING NESTED TYPE error because the associated type is not yet in scope while the enclosing type's own annotation is canonicalized.
type=file:ModuleType.roc
~~~
# SOURCE
~~~roc
ModuleType := {
    field : ModuleType.InternalType,
}.{
    InternalType := [Some, Other]
}
~~~
# EXPECTED
MISSING NESTED TYPE - type_module_nominal_field_depends_on_nested_type.md:2:13:2:36
# PROBLEMS
**MISSING NESTED TYPE**
`ModuleType` is in scope, but it doesn't have a nested type named `InternalType`.

It's referenced here:
**type_module_nominal_field_depends_on_nested_type.md:2:13:2:36:**
```roc
    field : ModuleType.InternalType,
```
            ^^^^^^^^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenCurly,
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
ModuleType := {
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
				(ty-malformed))))
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

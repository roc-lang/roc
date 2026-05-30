# META
~~~ini
description=Nominal (non-opaque) type module whose field depends on a PRIVATE top-level nominal type. Because ModuleType is declared with := its fields are public, but InternalType is not exposed to other modules, so this should warn. Marked skip=true until that warning is implemented; remove skip=true to activate the test.
type=file:ModuleType.roc
skip=true
# TODO: expected "PRIVATE TYPE IN EXPOSED FIELD" warning is not implemented yet
~~~
# SOURCE
~~~roc
InternalType := [Some, Other]

ModuleType := {
    field : InternalType,
}
~~~
# EXPECTED
PRIVATE TYPE IN EXPOSED FIELD - type_module_nominal_field_depends_on_private_toplevel_type.md:4:13:4:25
# PROBLEMS
**PRIVATE TYPE IN EXPOSED FIELD**
The `field` field of `ModuleType` refers to `InternalType`, but `InternalType` is private to this module, so other modules that use `ModuleType` cannot name this type.

It's referenced here:
**type_module_nominal_field_depends_on_private_toplevel_type.md:4:13:4:25:**
```roc
    field : InternalType,
```
            ^^^^^^^^^^^^

Because `ModuleType` is a nominal type (declared with `:=`), its fields are visible to other modules. To keep `InternalType` private, either make `ModuleType` opaque (declare it with `::`), or move `InternalType` into `ModuleType`'s associated block so it can be referenced as `ModuleType.InternalType`.


# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,
UpperIdent,OpColonEqual,OpenCurly,
LowerIdent,OpColon,UpperIdent,Comma,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "InternalType")
				(args))
			(ty-tag-union
				(tags
					(ty (name "Some"))
					(ty (name "Other")))))
		(s-type-decl
			(header (name "ModuleType")
				(args))
			(ty-record
				(anno-record-field (name "field")
					(ty (name "InternalType")))))))
~~~
# FORMATTED
~~~roc
InternalType := [Some, Other]

ModuleType := {
	field : InternalType,
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-nominal-decl
		(ty-header (name "InternalType"))
		(ty-tag-union
			(ty-tag-name (name "Some"))
			(ty-tag-name (name "Other"))))
	(s-nominal-decl
		(ty-header (name "ModuleType"))
		(ty-record
			(field (field "field")
				(ty-lookup (name "InternalType") (local))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(nominal (type "InternalType")
			(ty-header (name "InternalType")))
		(nominal (type "ModuleType")
			(ty-header (name "ModuleType"))))
	(expressions))
~~~

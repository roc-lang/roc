# META
~~~ini
description=Nominal (non-opaque) type module whose field depends on a PRIVATE top-level nominal type. Because ModuleType is declared with := its fields are public, but InternalType is not exposed to other modules, so this warns.
type=file:ModuleType.roc
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
The `field` field of _ModuleType_ refers to _InternalType_, but _InternalType_ is private to this module.

Other modules can see this field because _ModuleType_ is exposed and not opaque, but they cannot name this private type.

It's referenced here:
**type_module_nominal_field_depends_on_private_toplevel_type.md:4:13:4:25:**
```roc
    field : InternalType,
```
            ^^^^^^^^^^^^


**Hint:** Expose the referenced type, make _ModuleType_ opaque with `::`, or move the type into _ModuleType_'s associated block.

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

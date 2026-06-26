# META
~~~ini
description=Nominal (non-opaque) type module whose field depends on a private top-level nominal type declared later in the same file. This covers issue #9486's top-level arrangement: it compiles, but warns because the private nominal appears in ModuleType's public surface.
type=file:ModuleType.roc
~~~
# SOURCE
~~~roc
ModuleType := {
    field : InternalType,
}

InternalType := [Some, Other]
~~~
# EXPECTED
PRIVATE TYPE IN EXPOSED FIELD - type_module_nominal_field_depends_on_later_private_toplevel_type.md:2:13:2:25
# PROBLEMS

┌───────────────────────────────┐
│ PRIVATE TYPE IN EXPOSED FIELD ├─ The `field` field of `ModuleType` refers ──┐
└┬──────────────────────────────┘  to `InternalType`, but `InternalType` is   │
 │                                 private to this module.                    │
 │                                                                            │
 │  field : InternalType,                                                     │
 │          ‾‾‾‾‾‾‾‾‾‾‾‾                                                      │
 └── type_module_nominal_field_depends_on_later_private_toplevel_type.md:2:13 ┘

    Other modules can see this field because ModuleType is exposed and not
    opaque, but they cannot name this private type.



    Hint: Expose the referenced type, make ModuleType opaque with `::`, or move
    the type into ModuleType's associated block.

# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenCurly,
LowerIdent,OpColon,UpperIdent,Comma,
CloseCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,
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
					(ty (name "InternalType")))))
		(s-type-decl
			(header (name "InternalType")
				(args))
			(ty-tag-union
				(tags
					(ty (name "Some"))
					(ty (name "Other")))))))
~~~
# FORMATTED
~~~roc
ModuleType := {
	field : InternalType,
}

InternalType := [Some, Other]
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
		(ty-header (name "InternalType"))
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
		(nominal (type "InternalType")
			(ty-header (name "InternalType"))))
	(expressions))
~~~

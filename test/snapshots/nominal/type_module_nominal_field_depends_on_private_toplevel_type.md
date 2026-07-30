# META
~~~ini
description=Nominal (non-opaque) type mod whose field depends on a PRIVATE top-level nominal type. Because ModType is declared with := its fields are public, but InternalType is not exposed to other mods, so this warns.
type=file:ModType.roc
~~~
# SOURCE
~~~roc
InternalType := [Some, Other]

ModType := {
    field : InternalType,
}
~~~
# EXPECTED
PRIVATE TYPE IN EXPOSED FIELD - type_mod_nominal_field_depends_on_private_toplevel_type.md:4:13:4:25
# PROBLEMS

┌───────────────────────────────┐
│ PRIVATE TYPE IN EXPOSED FIELD ├─ The `field` field of `ModType` refers to ──┐
└┬──────────────────────────────┘  `InternalType`, but `InternalType` is      │
 │                                 private to this mod.                    │
 │                                                                            │
 │  field : InternalType,                                                     │
 │          ‾‾‾‾‾‾‾‾‾‾‾‾                                                      │
 └──────── type_mod_nominal_field_depends_on_private_toplevel_type.md:4:13 ┘

    Other mods can see this field because ModType is exposed and not opaque,
    but they cannot name this private type.



    Hint: Expose the referenced type, make ModType opaque with `::`, or move
    the type into ModType's associated block.

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
InternalType := [Some, Other]

ModType := {
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
		(nominal (type "InternalType")
			(ty-header (name "InternalType")))
		(nominal (type "ModType")
			(ty-header (name "ModType"))))
	(expressions))
~~~

# META
~~~ini
description=Nominal (non-opaque) type mod whose field depends on a private top-level nominal type declared later in the same file. This covers issue #9486's top-level arrangement: it compiles, but warns because the private nominal appears in ModType's public surface.
type=file:ModType.roc
~~~
# SOURCE
~~~roc
ModType := {
    field : InternalType,
}

InternalType := [Some, Other]
~~~
# EXPECTED
PRIVATE TYPE IN EXPOSED FIELD - type_mod_nominal_field_depends_on_later_private_toplevel_type.md:2:13:2:25
# PROBLEMS

┌───────────────────────────────┐
│ PRIVATE TYPE IN EXPOSED FIELD ├─ The `field` field of `ModType` refers to ──┐
└┬──────────────────────────────┘  `InternalType`, but `InternalType` is      │
 │                                 private to this mod.                    │
 │                                                                            │
 │  field : InternalType,                                                     │
 │          ‾‾‾‾‾‾‾‾‾‾‾‾                                                      │
 └── type_mod_nominal_field_depends_on_later_private_toplevel_type.md:2:13 ┘

    Other mods can see this field because ModType is exposed and not opaque,
    but they cannot name this private type.



    Hint: Expose the referenced type, make ModType opaque with `::`, or move
    the type into ModType's associated block.

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
	(type-mod)
	(statements
		(s-type-decl
			(header (name "ModType")
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
ModType := {
	field : InternalType,
}

InternalType := [Some, Other]
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
		(nominal (type "ModType")
			(ty-header (name "ModType")))
		(nominal (type "InternalType")
			(ty-header (name "InternalType"))))
	(expressions))
~~~

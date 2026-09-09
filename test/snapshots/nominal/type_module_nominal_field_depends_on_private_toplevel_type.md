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
~~~clojure
(reports
	(report
		(severity warning)
		(title "Private Type In Exposed Field")
		(region (start 4 13) (end 4 25))
		(headline
			(reflow "The ")
			(annotated symbol-unqualified "field")
			(reflow " field of ")
			(annotated code "ModType")
			(reflow " refers to ")
			(annotated code "InternalType")
			(reflow ", but ")
			(annotated code "InternalType")
			(reflow " is private to this mod."))
		(document
			(reflow "Other mods can see this field because ")
			(annotated type "ModType")
			(reflow " is exposed and not opaque, but they cannot name this private type.")
			(line-break)
			(line-break)
			(source-region (file "type_mod_nominal_field_depends_on_private_toplevel_type.md") (start 4 13) (end 4 25) (annotation warning) (line-text "    field : InternalType,"))
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " Expose the referenced type, make ")
			(annotated type "ModType")
			(reflow " opaque with ")
			(annotated code "::")
			(reflow ", or move the type into ")
			(annotated type "ModType")
			(reflow "'s associated block."))))
~~~
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

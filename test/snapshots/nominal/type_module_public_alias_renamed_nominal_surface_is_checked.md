# META
~~~ini
description=A non-opaque private nominal type renamed by a public alias is public, so its own backing is checked and a private type in it warns under the alias's name
type=file:Api.roc
~~~
# SOURCE
~~~roc
Api := { child : Api.Child }.{
    Child : ChildValue
}
ChildValue := { secret : Secret }
Secret := [Hidden]
~~~
# EXPECTED
PRIVATE TYPE IN EXPOSED FIELD - type_mod_public_alias_renamed_nominal_surface_is_checked.md:4:26:4:32
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Private Type In Exposed Field")
		(region (start 4 26) (end 4 32))
		(headline
			(reflow "The ")
			(annotated symbol-unqualified "secret")
			(reflow " field of ")
			(annotated code "Api.Child")
			(reflow " refers to ")
			(annotated code "Secret")
			(reflow ", but ")
			(annotated code "Secret")
			(reflow " is private to this mod."))
		(document
			(reflow "Other mods can see this field because ")
			(annotated type "Api.Child")
			(reflow " is exposed and not opaque, but they cannot name this private type.")
			(line-break)
			(line-break)
			(source-region (file "type_mod_public_alias_renamed_nominal_surface_is_checked.md") (start 4 26) (end 4 32) (annotation warning) (line-text "ChildValue := { secret : Secret }"))
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " Expose the referenced type, make ")
			(annotated type "Api.Child")
			(reflow " opaque with ")
			(annotated code "::")
			(reflow ", or move the type into ")
			(annotated type "Api.Child")
			(reflow "'s associated block."))))
~~~
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenCurly,LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,CloseCurly,Dot,OpenCurly,
UpperIdent,OpColon,UpperIdent,
CloseCurly,
UpperIdent,OpColonEqual,OpenCurly,LowerIdent,OpColon,UpperIdent,CloseCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Api")
				(args))
			(ty-record
				(anno-record-field (name "child")
					(ty (name "Api.Child"))))
			(associated
				(s-type-decl
					(header (name "Child")
						(args))
					(ty (name "ChildValue")))))
		(s-type-decl
			(header (name "ChildValue")
				(args))
			(ty-record
				(anno-record-field (name "secret")
					(ty (name "Secret")))))
		(s-type-decl
			(header (name "Secret")
				(args))
			(ty-tag-union
				(tags
					(ty (name "Hidden")))))))
~~~
# FORMATTED
~~~roc
Api := { child : Api.Child }.{
	Child : ChildValue
}

ChildValue := { secret : Secret }

Secret := [Hidden]
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-nominal-decl
		(ty-header (name "Api"))
		(ty-record
			(field (field "child")
				(ty-lookup (name "Api.Child") (local)))))
	(s-alias-decl
		(ty-header (name "Api.Child"))
		(ty-lookup (name "ChildValue") (local)))
	(s-nominal-decl
		(ty-header (name "ChildValue"))
		(ty-record
			(field (field "secret")
				(ty-lookup (name "Secret") (local)))))
	(s-nominal-decl
		(ty-header (name "Secret"))
		(ty-tag-union
			(ty-tag-name (name "Hidden")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(nominal (type "Api")
			(ty-header (name "Api")))
		(alias (type "Api.Child")
			(ty-header (name "Api.Child")))
		(nominal (type "ChildValue")
			(ty-header (name "ChildValue")))
		(nominal (type "Secret")
			(ty-header (name "Secret"))))
	(expressions))
~~~

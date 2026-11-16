# META
~~~ini
description=Renaming publishes a private nominal type's own name only, so a type nested in its block still warns when an exposed field names it
type=file:Api.roc
~~~
# SOURCE
~~~roc
Api := { inner : ChildValue.Inner, child : Api.Child }.{
    Child : ChildValue
}
ChildValue := [One].{
    Inner := [Nested]
}
~~~
# EXPECTED
PRIVATE TYPE IN EXPOSED FIELD - type_mod_nested_type_of_renamed_nominal_still_warns.md:1:18:1:34
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Private Type In Exposed Field")
		(region (start 1 18) (end 1 34))
		(headline
			(reflow "The ")
			(annotated symbol-unqualified "inner")
			(reflow " field of ")
			(annotated code "Api")
			(reflow " refers to ")
			(annotated code "Api.ChildValue.Inner")
			(reflow ", but ")
			(annotated code "Api.ChildValue.Inner")
			(reflow " is private to this mod."))
		(document
			(reflow "Other mods can see this field because ")
			(annotated type "Api")
			(reflow " is exposed and not opaque, but they cannot name this private type.")
			(line-break)
			(line-break)
			(source-region (file "type_mod_nested_type_of_renamed_nominal_still_warns.md") (start 1 18) (end 1 34) (annotation warning) (line-text "Api := { inner : ChildValue.Inner, child : Api.Child }.{"))
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " Expose the referenced type, make ")
			(annotated type "Api")
			(reflow " opaque with ")
			(annotated code "::")
			(reflow ", or move the type into ")
			(annotated type "Api")
			(reflow "'s associated block."))))
~~~
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenCurly,LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,Comma,LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,CloseCurly,Dot,OpenCurly,
UpperIdent,OpColon,UpperIdent,
CloseCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,
CloseCurly,
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
				(anno-record-field (name "inner")
					(ty (name "ChildValue.Inner")))
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
			(ty-tag-union
				(tags
					(ty (name "One"))))
			(associated
				(s-type-decl
					(header (name "Inner")
						(args))
					(ty-tag-union
						(tags
							(ty (name "Nested")))))))))
~~~
# FORMATTED
~~~roc
Api := { inner : ChildValue.Inner, child : Api.Child }.{
	Child : ChildValue
}

ChildValue := [One].{
	Inner := [Nested]
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-nominal-decl
		(ty-header (name "Api"))
		(ty-record
			(field (field "inner")
				(ty-lookup (name "ChildValue.Inner") (local)))
			(field (field "child")
				(ty-lookup (name "Api.Child") (local)))))
	(s-alias-decl
		(ty-header (name "Api.Child"))
		(ty-lookup (name "ChildValue") (local)))
	(s-nominal-decl
		(ty-header (name "ChildValue"))
		(ty-tag-union
			(ty-tag-name (name "One"))))
	(s-nominal-decl
		(ty-header (name "Api.ChildValue.Inner"))
		(ty-tag-union
			(ty-tag-name (name "Nested")))))
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
		(nominal (type "ChildValue.Inner")
			(ty-header (name "Api.ChildValue.Inner"))))
	(expressions))
~~~

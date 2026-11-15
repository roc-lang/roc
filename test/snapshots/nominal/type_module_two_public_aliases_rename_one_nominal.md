# META
~~~ini
description=When two public aliases rename one private nominal type, the one declared first names it, and its backing is checked once
type=file:Api.roc
~~~
# SOURCE
~~~roc
Api := { a : Api.First, b : Api.Second }.{
    First : ChildValue
    Second : ChildValue
}
ChildValue := { secret : Secret }
Secret := [Hidden]
~~~
# EXPECTED
PRIVATE TYPE IN EXPOSED FIELD - type_mod_two_public_aliases_rename_one_nominal.md:5:26:5:32
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Private Type In Exposed Field")
		(region (start 5 26) (end 5 32))
		(headline
			(reflow "The ")
			(annotated symbol-unqualified "secret")
			(reflow " field of ")
			(annotated code "Api.First")
			(reflow " refers to ")
			(annotated code "Secret")
			(reflow ", but ")
			(annotated code "Secret")
			(reflow " is private to this mod."))
		(document
			(reflow "Other mods can see this field because ")
			(annotated type "Api.First")
			(reflow " is exposed and not opaque, but they cannot name this private type.")
			(line-break)
			(line-break)
			(source-region (file "type_mod_two_public_aliases_rename_one_nominal.md") (start 5 26) (end 5 32) (annotation warning) (line-text "ChildValue := { secret : Secret }"))
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " Expose the referenced type, make ")
			(annotated type "Api.First")
			(reflow " opaque with ")
			(annotated code "::")
			(reflow ", or move the type into ")
			(annotated type "Api.First")
			(reflow "'s associated block."))))
~~~
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenCurly,LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,Comma,LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,CloseCurly,Dot,OpenCurly,
UpperIdent,OpColon,UpperIdent,
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
				(anno-record-field (name "a")
					(ty (name "Api.First")))
				(anno-record-field (name "b")
					(ty (name "Api.Second"))))
			(associated
				(s-type-decl
					(header (name "First")
						(args))
					(ty (name "ChildValue")))
				(s-type-decl
					(header (name "Second")
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
Api := { a : Api.First, b : Api.Second }.{
	First : ChildValue
	Second : ChildValue
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
			(field (field "a")
				(ty-lookup (name "Api.First") (local)))
			(field (field "b")
				(ty-lookup (name "Api.Second") (local)))))
	(s-alias-decl
		(ty-header (name "Api.First"))
		(ty-lookup (name "ChildValue") (local)))
	(s-alias-decl
		(ty-header (name "Api.Second"))
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
		(alias (type "Api.First")
			(ty-header (name "Api.First")))
		(alias (type "Api.Second")
			(ty-header (name "Api.Second")))
		(nominal (type "ChildValue")
			(ty-header (name "ChildValue")))
		(nominal (type "Secret")
			(ty-header (name "Secret"))))
	(expressions))
~~~

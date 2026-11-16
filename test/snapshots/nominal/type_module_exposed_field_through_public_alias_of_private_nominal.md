# META
~~~ini
description=Exposed record field whose type is a public alias of a private nominal type does not warn, because other mods name that type through the alias (https://github.com/roc-lang/roc/issues/13023)
type=file:PublicField.roc
~~~
# SOURCE
~~~roc
PublicField := { child : PublicField.Child }.{
    Child : ChildValue
}
ChildValue := [One]
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenCurly,LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,CloseCurly,Dot,OpenCurly,
UpperIdent,OpColon,UpperIdent,
CloseCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "PublicField")
				(args))
			(ty-record
				(anno-record-field (name "child")
					(ty (name "PublicField.Child"))))
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
					(ty (name "One")))))))
~~~
# FORMATTED
~~~roc
PublicField := { child : PublicField.Child }.{
	Child : ChildValue
}

ChildValue := [One]
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-nominal-decl
		(ty-header (name "PublicField"))
		(ty-record
			(field (field "child")
				(ty-lookup (name "PublicField.Child") (local)))))
	(s-alias-decl
		(ty-header (name "PublicField.Child"))
		(ty-lookup (name "ChildValue") (local)))
	(s-nominal-decl
		(ty-header (name "ChildValue"))
		(ty-tag-union
			(ty-tag-name (name "One")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(nominal (type "PublicField")
			(ty-header (name "PublicField")))
		(alias (type "PublicField.Child")
			(ty-header (name "PublicField.Child")))
		(nominal (type "ChildValue")
			(ty-header (name "ChildValue"))))
	(expressions))
~~~

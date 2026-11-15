# META
~~~ini
description=An opaque private nominal type renamed by a public alias exposes no backing, so a private type in its backing does not warn
type=file:Api.roc
~~~
# SOURCE
~~~roc
Api := { child : Api.Child }.{
    Child : ChildValue
}
ChildValue :: { secret : Secret }
Secret := [Hidden]
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
UpperIdent,OpDoubleColon,OpenCurly,LowerIdent,OpColon,UpperIdent,CloseCurly,
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

ChildValue :: { secret : Secret }

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

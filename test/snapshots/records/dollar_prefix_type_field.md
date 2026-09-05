# META
~~~ini
description=Dollar-prefixed record type field names are preserved
type=statement
~~~
# SOURCE
~~~roc
Person : { $name : Str }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(s-type-decl
	(header (name "Person")
		(args))
	(ty-record
		(anno-record-field (name "$name")
			(ty (name "Str")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl
		(ty-header (name "Person"))
		(ty-record
			(field (field "$name")
				(ty-lookup (name "Str") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias (type "Person")
			(ty-header (name "Person"))))
	(expressions))
~~~

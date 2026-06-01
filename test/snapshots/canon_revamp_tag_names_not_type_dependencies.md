# META
~~~ini
description=Tag constructor names do not count as type alias dependencies
type=snippet
~~~
# SOURCE
~~~roc
A : [B]
B : A
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColon,OpenSquare,UpperIdent,CloseSquare,
UpperIdent,OpColon,UpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "A")
				(args))
			(ty-tag-union
				(tags
					(ty (name "B")))))
		(s-type-decl
			(header (name "B")
				(args))
			(ty (name "A")))))
~~~
# FORMATTED
~~~roc
A : [B]

B : A
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl
		(ty-header (name "A"))
		(ty-tag-union
			(ty-tag-name (name "B"))))
	(s-alias-decl
		(ty-header (name "B"))
		(ty-lookup (name "A") (local))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias (type "A")
			(ty-header (name "A")))
		(alias (type "B")
			(ty-header (name "B"))))
	(expressions))
~~~

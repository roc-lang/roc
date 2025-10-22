# META
~~~ini
description=Multiple unqualified type declarations
type=snippet
~~~
# SOURCE
~~~roc
FirstType : U64
SecondType : Str
ThirdType : List(U8)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColon,UpperIdent,
UpperIdent,OpColon,UpperIdent,
UpperIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "FirstType")
				(args))
			(ty (name "U64")))
		(s-type-decl
			(header (name "SecondType")
				(args))
			(ty (name "Str")))
		(s-type-decl
			(header (name "ThirdType")
				(args))
			(ty-apply
				(ty (name "List"))
				(ty (name "U8"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl
		(ty-header (name "FirstType"))
		(ty-lookup (name "U64") (builtin)))
	(s-alias-decl
		(ty-header (name "SecondType"))
		(ty-lookup (name "Str") (external-module "Str")))
	(s-alias-decl
		(ty-header (name "ThirdType"))
		(ty-apply (name "List") (builtin)
			(ty-lookup (name "U8") (builtin)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias (type "FirstType")
			(ty-header (name "FirstType")))
		(alias (type "SecondType")
			(ty-header (name "SecondType")))
		(alias (type "ThirdType")
			(ty-header (name "ThirdType"))))
	(expressions))
~~~

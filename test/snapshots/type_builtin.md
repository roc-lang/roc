# META
~~~ini
description=Simple test for built-in types in scope
type=snippet
~~~
# SOURCE
~~~roc
MyNumber : U64
MyString : Str
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColon,UpperIdent,
UpperIdent,OpColon,UpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "MyNumber")
				(args))
			(ty (name "U64")))
		(s-type-decl
			(header (name "MyString")
				(args))
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
		(ty-header (name "MyNumber"))
		(ty-lookup (name "U64") (builtin)))
	(s-alias-decl
		(ty-header (name "MyString"))
		(ty-lookup (name "Str") (external-module "Str"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias (type "MyNumber")
			(ty-header (name "MyNumber")))
		(alias (type "MyString")
			(ty-header (name "MyString"))))
	(expressions))
~~~

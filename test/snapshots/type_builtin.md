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
UpperIdent(1:1-1:9),OpColon(1:10-1:11),UpperIdent(1:12-1:15),
UpperIdent(2:1-2:9),OpColon(2:10-2:11),UpperIdent(2:12-2:15),
EndOfFile(3:1-3:1),
~~~
# PARSE
~~~clojure
(file @1.1-2.15
	(type-module @1.1-1.9)
	(statements
		(s-type-decl @1.1-1.15
			(header @1.1-1.9 (name "MyNumber")
				(args))
			(ty @1.12-1.15 (name "U64")))
		(s-type-decl @2.1-2.15
			(header @2.1-2.9 (name "MyString")
				(args))
			(ty @2.12-2.15 (name "Str")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl @1.1-1.15
		(ty-header @1.1-1.9 (name "MyNumber"))
		(ty-lookup @1.12-1.15 (name "U64") (builtin)))
	(s-alias-decl @2.1-2.15
		(ty-header @2.1-2.9 (name "MyString"))
		(ty-lookup @2.12-2.15 (name "Str") (builtin))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias @1.1-1.15 (type "MyNumber")
			(ty-header @1.1-1.9 (name "MyNumber")))
		(alias @2.1-2.15 (type "MyString")
			(ty-header @2.1-2.9 (name "MyString"))))
	(expressions))
~~~

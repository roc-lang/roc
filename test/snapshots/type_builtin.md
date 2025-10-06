# META
~~~ini
description=Simple test for built-in types in scope
type=file:TypeBuiltin.roc
~~~
# SOURCE
~~~roc
TypeBuiltin := {}

MyNumber : U64
MyString : Str
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:12),OpColonEqual(1:13-1:15),OpenCurly(1:16-1:17),CloseCurly(1:17-1:18),
UpperIdent(3:1-3:9),OpColon(3:10-3:11),UpperIdent(3:12-3:15),
UpperIdent(4:1-4:9),OpColon(4:10-4:11),UpperIdent(4:12-4:15),
EndOfFile(5:1-5:1),
~~~
# PARSE
~~~clojure
(file @1.1-4.15
	(type-module @1.1-1.12)
	(statements
		(s-type-decl @1.1-1.18
			(header @1.1-1.12 (name "TypeBuiltin")
				(args))
			(ty-record @1.16-1.18))
		(s-type-decl @3.1-3.15
			(header @3.1-3.9 (name "MyNumber")
				(args))
			(ty @3.12-3.15 (name "U64")))
		(s-type-decl @4.1-4.15
			(header @4.1-4.9 (name "MyString")
				(args))
			(ty @4.12-4.15 (name "Str")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-nominal-decl @1.1-1.18
		(ty-header @1.1-1.12 (name "TypeBuiltin"))
		(ty-record @1.16-1.18))
	(s-alias-decl @3.1-3.15
		(ty-header @3.1-3.9 (name "MyNumber"))
		(ty-lookup @3.12-3.15 (name "U64") (builtin)))
	(s-alias-decl @4.1-4.15
		(ty-header @4.1-4.9 (name "MyString"))
		(ty-lookup @4.12-4.15 (name "Str") (builtin))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(nominal @1.1-1.18 (type "TypeBuiltin")
			(ty-header @1.1-1.12 (name "TypeBuiltin")))
		(alias @3.1-3.15 (type "MyNumber")
			(ty-header @3.1-3.9 (name "MyNumber")))
		(alias @4.1-4.15 (type "MyString")
			(ty-header @4.1-4.9 (name "MyString"))))
	(expressions))
~~~

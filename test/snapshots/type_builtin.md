# META
~~~ini
description=Simple test for built-in types in scope
type=file
~~~
# SOURCE
~~~roc
module [MyNumber, MyString]

MyNumber : U64
MyString : Str
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:17),Comma(1:17-1:18),UpperIdent(1:19-1:27),CloseSquare(1:27-1:28),
UpperIdent(3:1-3:9),OpColon(3:10-3:11),UpperIdent(3:12-3:15),
UpperIdent(4:1-4:9),OpColon(4:10-4:11),UpperIdent(4:12-4:15),
EndOfFile(5:1-5:1),
~~~
# PARSE
~~~clojure
(file @1.1-4.15
	(module @1.1-1.28
		(exposes @1.8-1.28
			(exposed-upper-ident @1.9-1.17 (text "MyNumber"))
			(exposed-upper-ident @1.19-1.27 (text "MyString"))))
	(statements
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
	(s-nominal-decl @1.1-1.1
		(ty-header @1.1-1.1 (name "Bool"))
		(ty-tag-union @1.1-1.1
			(tag_name @1.1-1.1 (name "True"))
			(tag_name @1.1-1.1 (name "False"))))
	(s-nominal-decl @1.1-1.1
		(ty-header @1.1-1.1 (name "Result")
			(ty-args
				(ty-rigid-var @1.1-1.1 (name "ok"))
				(ty-rigid-var @1.1-1.1 (name "err"))))
		(ty-tag-union @1.1-1.1
			(tag_name @1.1-1.1 (name "Ok"))
			(tag_name @1.1-1.1 (name "Err"))))
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
		(nominal @1.1-1.1 (type "Bool")
			(ty-header @1.1-1.1 (name "Bool")))
		(nominal @1.1-1.1 (type "Result(ok, err)")
			(ty-header @1.1-1.1 (name "Result")
				(ty-args
					(ty-rigid-var @1.1-1.1 (name "ok"))
					(ty-rigid-var @1.1-1.1 (name "err")))))
		(alias @3.1-3.15 (type "MyNumber")
			(ty-header @3.1-3.9 (name "MyNumber")))
		(alias @4.1-4.15 (type "MyString")
			(ty-header @4.1-4.9 (name "MyString"))))
	(expressions))
~~~

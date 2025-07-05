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
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:17),Comma(1:17-1:18),UpperIdent(1:19-1:27),CloseSquare(1:27-1:28),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(3:1-3:9),OpColon(3:10-3:11),UpperIdent(3:12-3:15),Newline(1:1-1:1),
UpperIdent(4:1-4:9),OpColon(4:10-4:11),UpperIdent(4:12-4:15),EndOfFile(4:15-4:15),
~~~
# PARSE
~~~clojure
(file @1.1-4.15
	(module @1.1-1.28
		(exposes @1.8-1.28
			(exposed-upper-ident (text "MyNumber"))
			(exposed-upper-ident (text "MyString"))))
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
	(s-alias-decl @3.1-3.15 (where "TODO")
		(ty-header @3.1-3.9 (name "MyNumber"))
		(ty @3.12-3.15 (name "U64")))
	(s-alias-decl @4.1-4.15 (where "TODO")
		(ty-header @4.1-4.9 (name "MyString"))
		(ty @4.12-4.15 (name "Str"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~

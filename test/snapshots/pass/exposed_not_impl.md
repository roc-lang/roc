# META
~~~ini
description=Module exposes values that are not implemented
type=file:ExposedNotImpl.roc
~~~
# SOURCE
~~~roc
ExposedNotImpl := {}

# This module exposes foo, bar, MyType, and OtherType
# but only implements foo and MyType
# This should generate "exposed but not implemented" errors for bar and OtherType
# Also tests redundant exposed entries for foo and MyType

foo = 42

MyType : [A, B, C]
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:15),OpColonEqual(1:16-1:18),OpenCurly(1:19-1:20),CloseCurly(1:20-1:21),
LowerIdent(8:1-8:4),OpAssign(8:5-8:6),Int(8:7-8:9),
UpperIdent(10:1-10:7),OpColon(10:8-10:9),OpenSquare(10:10-10:11),UpperIdent(10:11-10:12),Comma(10:12-10:13),UpperIdent(10:14-10:15),Comma(10:15-10:16),UpperIdent(10:17-10:18),CloseSquare(10:18-10:19),
EndOfFile(11:1-11:1),
~~~
# PARSE
~~~clojure
(file @1.1-10.19
	(type-module @1.1-1.15)
	(statements
		(s-type-decl @1.1-1.21
			(header @1.1-1.15 (name "ExposedNotImpl")
				(args))
			(ty-record @1.19-1.21))
		(s-decl @8.1-8.9
			(p-ident @8.1-8.4 (raw "foo"))
			(e-int @8.7-8.9 (raw "42")))
		(s-type-decl @10.1-10.19
			(header @10.1-10.7 (name "MyType")
				(args))
			(ty-tag-union @10.10-10.19
				(tags
					(ty @10.11-10.12 (name "A"))
					(ty @10.14-10.15 (name "B"))
					(ty @10.17-10.18 (name "C")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @8.1-8.4 (ident "foo"))
		(e-num @8.7-8.9 (value "42")))
	(s-nominal-decl @1.1-1.21
		(ty-header @1.1-1.15 (name "ExposedNotImpl"))
		(ty-record @1.19-1.21))
	(s-alias-decl @10.1-10.19
		(ty-header @10.1-10.7 (name "MyType"))
		(ty-tag-union @10.10-10.19
			(ty-tag-name @10.11-10.12 (name "A"))
			(ty-tag-name @10.14-10.15 (name "B"))
			(ty-tag-name @10.17-10.18 (name "C")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @8.1-8.4 (type "Num(_size)")))
	(type_decls
		(nominal @1.1-1.21 (type "ExposedNotImpl")
			(ty-header @1.1-1.15 (name "ExposedNotImpl")))
		(alias @10.1-10.19 (type "MyType")
			(ty-header @10.1-10.7 (name "MyType"))))
	(expressions
		(expr @8.7-8.9 (type "Num(_size)"))))
~~~

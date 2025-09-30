# META
~~~ini
description=Test qualified tag with type annotation
type=file:MyType.roc
~~~
# SOURCE
~~~roc
MyType := [TagA, TagB]

value : MyType
value = MyType.TagA
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:7),OpColonEqual(1:8-1:10),OpenSquare(1:11-1:12),UpperIdent(1:12-1:16),Comma(1:16-1:17),UpperIdent(1:18-1:22),CloseSquare(1:22-1:23),
LowerIdent(3:1-3:6),OpColon(3:7-3:8),UpperIdent(3:9-3:15),
LowerIdent(4:1-4:6),OpAssign(4:7-4:8),UpperIdent(4:9-4:15),NoSpaceDotUpperIdent(4:15-4:20),
EndOfFile(5:1-5:1),
~~~
# PARSE
~~~clojure
(file @1.1-4.20
	(type-module @1.1-1.7)
	(statements
		(s-type-decl @1.1-1.23
			(header @1.1-1.7 (name "MyType")
				(args))
			(ty-tag-union @1.11-1.23
				(tags
					(ty @1.12-1.16 (name "TagA"))
					(ty @1.18-1.22 (name "TagB")))))
		(s-type-anno @3.1-3.15 (name "value")
			(ty @3.9-3.15 (name "MyType")))
		(s-decl @4.1-4.20
			(p-ident @4.1-4.6 (raw "value"))
			(e-tag @4.9-4.20 (raw "MyType.TagA")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.6 (ident "value"))
		(e-nominal @4.9-4.20 (nominal "MyType")
			(e-tag @4.9-4.20 (name "TagA")))
		(annotation @4.1-4.6
			(declared-type
				(ty @3.9-3.15 (name "MyType")))))
	(s-nominal-decl @1.1-1.23
		(ty-header @1.1-1.7 (name "MyType"))
		(ty-tag-union @1.11-1.23
			(ty @1.12-1.16 (name "TagA"))
			(ty @1.18-1.22 (name "TagB")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.6 (type "MyType")))
	(type_decls
		(nominal @1.1-1.23 (type "MyType")
			(ty-header @1.1-1.7 (name "MyType"))))
	(expressions
		(expr @4.9-4.20 (type "MyType"))))
~~~

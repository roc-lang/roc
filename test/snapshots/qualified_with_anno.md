# META
~~~ini
description=Test qualified tag with type annotation
type=file:QualifiedWithAnno.roc
~~~
# SOURCE
~~~roc
QualifiedWithAnno := {}

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
UpperIdent(1:1-1:18),OpColonEqual(1:19-1:21),OpenCurly(1:22-1:23),CloseCurly(1:23-1:24),
UpperIdent(3:1-3:7),OpColonEqual(3:8-3:10),OpenSquare(3:11-3:12),UpperIdent(3:12-3:16),Comma(3:16-3:17),UpperIdent(3:18-3:22),CloseSquare(3:22-3:23),
LowerIdent(5:1-5:6),OpColon(5:7-5:8),UpperIdent(5:9-5:15),
LowerIdent(6:1-6:6),OpAssign(6:7-6:8),UpperIdent(6:9-6:15),NoSpaceDotUpperIdent(6:15-6:20),
EndOfFile(7:1-7:1),
~~~
# PARSE
~~~clojure
(file @1.1-6.20
	(type-module @1.1-1.18)
	(statements
		(s-type-decl @1.1-1.24
			(header @1.1-1.18 (name "QualifiedWithAnno")
				(args))
			(ty-record @1.22-1.24))
		(s-type-decl @3.1-3.23
			(header @3.1-3.7 (name "MyType")
				(args))
			(ty-tag-union @3.11-3.23
				(tags
					(ty @3.12-3.16 (name "TagA"))
					(ty @3.18-3.22 (name "TagB")))))
		(s-type-anno @5.1-5.15 (name "value")
			(ty @5.9-5.15 (name "MyType")))
		(s-decl @6.1-6.20
			(p-ident @6.1-6.6 (raw "value"))
			(e-tag @6.9-6.20 (raw "MyType.TagA")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @6.1-6.6 (ident "value"))
		(e-nominal @6.9-6.20 (nominal "MyType")
			(e-tag @6.9-6.20 (name "TagA")))
		(annotation @6.1-6.6
			(declared-type
				(ty-lookup @5.9-5.15 (name "MyType") (local)))))
	(s-nominal-decl @1.1-1.24
		(ty-header @1.1-1.18 (name "QualifiedWithAnno"))
		(ty-record @1.22-1.24))
	(s-nominal-decl @3.1-3.23
		(ty-header @3.1-3.7 (name "MyType"))
		(ty-tag-union @3.11-3.23
			(ty-tag-name @3.12-3.16 (name "TagA"))
			(ty-tag-name @3.18-3.22 (name "TagB")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.6 (type "MyType")))
	(type_decls
		(nominal @1.1-1.24 (type "QualifiedWithAnno")
			(ty-header @1.1-1.18 (name "QualifiedWithAnno")))
		(nominal @3.1-3.23 (type "MyType")
			(ty-header @3.1-3.7 (name "MyType"))))
	(expressions
		(expr @6.9-6.20 (type "MyType"))))
~~~

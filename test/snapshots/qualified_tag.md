# META
~~~ini
description=Simple qualified tag test
type=file:QualifiedTag.roc
~~~
# SOURCE
~~~roc
QualifiedTag := {}

Color := [Red, Blue]

test = Color.Red
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:13),OpColonEqual(1:14-1:16),OpenCurly(1:17-1:18),CloseCurly(1:18-1:19),
UpperIdent(3:1-3:6),OpColonEqual(3:7-3:9),OpenSquare(3:10-3:11),UpperIdent(3:11-3:14),Comma(3:14-3:15),UpperIdent(3:16-3:20),CloseSquare(3:20-3:21),
LowerIdent(5:1-5:5),OpAssign(5:6-5:7),UpperIdent(5:8-5:13),NoSpaceDotUpperIdent(5:13-5:17),
EndOfFile(6:1-6:1),
~~~
# PARSE
~~~clojure
(file @1.1-5.17
	(type-module @1.1-1.13)
	(statements
		(s-type-decl @1.1-1.19
			(header @1.1-1.13 (name "QualifiedTag")
				(args))
			(ty-record @1.17-1.19))
		(s-type-decl @3.1-3.21
			(header @3.1-3.6 (name "Color")
				(args))
			(ty-tag-union @3.10-3.21
				(tags
					(ty @3.11-3.14 (name "Red"))
					(ty @3.16-3.20 (name "Blue")))))
		(s-decl @5.1-5.17
			(p-ident @5.1-5.5 (raw "test"))
			(e-tag @5.8-5.17 (raw "Color.Red")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @5.1-5.5 (ident "test"))
		(e-nominal @5.8-5.17 (nominal "Color")
			(e-tag @5.8-5.17 (name "Red"))))
	(s-nominal-decl @1.1-1.19
		(ty-header @1.1-1.13 (name "QualifiedTag"))
		(ty-record @1.17-1.19))
	(s-nominal-decl @3.1-3.21
		(ty-header @3.1-3.6 (name "Color"))
		(ty-tag-union @3.10-3.21
			(ty-tag-name @3.11-3.14 (name "Red"))
			(ty-tag-name @3.16-3.20 (name "Blue")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @5.1-5.5 (type "Color")))
	(type_decls
		(nominal @1.1-1.19 (type "QualifiedTag")
			(ty-header @1.1-1.13 (name "QualifiedTag")))
		(nominal @3.1-3.21 (type "Color")
			(ty-header @3.1-3.6 (name "Color"))))
	(expressions
		(expr @5.8-5.17 (type "Color"))))
~~~

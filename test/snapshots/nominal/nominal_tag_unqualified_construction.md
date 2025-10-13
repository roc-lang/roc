# META
~~~ini
description=Construct nominal tag union value with unqualified tag
type=snippet
~~~
# SOURCE
~~~roc
Color := [Red, Green, Blue]

myColor : Color
myColor = Red
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:6),OpColonEqual(1:7-1:9),OpenSquare(1:10-1:11),UpperIdent(1:11-1:14),Comma(1:14-1:15),UpperIdent(1:16-1:21),Comma(1:21-1:22),UpperIdent(1:23-1:27),CloseSquare(1:27-1:28),
LowerIdent(3:1-3:8),OpColon(3:9-3:10),UpperIdent(3:11-3:16),
LowerIdent(4:1-4:8),OpAssign(4:9-4:10),UpperIdent(4:11-4:14),
EndOfFile(5:1-5:1),
~~~
# PARSE
~~~clojure
(file @1.1-4.14
	(type-module @1.1-1.6)
	(statements
		(s-type-decl @1.1-1.28
			(header @1.1-1.6 (name "Color")
				(args))
			(ty-tag-union @1.10-1.28
				(tags
					(ty @1.11-1.14 (name "Red"))
					(ty @1.16-1.21 (name "Green"))
					(ty @1.23-1.27 (name "Blue")))))
		(s-type-anno @3.1-3.16 (name "myColor")
			(ty @3.11-3.16 (name "Color")))
		(s-decl @4.1-4.14
			(p-ident @4.1-4.8 (raw "myColor"))
			(e-tag @4.11-4.14 (raw "Red")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.8 (ident "myColor"))
		(e-tag @4.11-4.14 (name "Red"))
		(annotation @4.1-4.8
			(declared-type
				(ty-lookup @3.11-3.16 (name "Color") (local)))))
	(s-nominal-decl @1.1-1.28
		(ty-header @1.1-1.6 (name "Color"))
		(ty-tag-union @1.10-1.28
			(ty-tag-name @1.11-1.14 (name "Red"))
			(ty-tag-name @1.16-1.21 (name "Green"))
			(ty-tag-name @1.23-1.27 (name "Blue")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.8 (type "Color")))
	(type_decls
		(nominal @1.1-1.28 (type "Color")
			(ty-header @1.1-1.6 (name "Color"))))
	(expressions
		(expr @4.11-4.14 (type "Color"))))
~~~

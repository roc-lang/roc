# META
~~~ini
description=Simple qualified tag test
type=snippet
~~~
# SOURCE
~~~roc
Color := [Red, Blue]

test = Color.Red
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:6),OpColonEqual(1:7-1:9),OpenSquare(1:10-1:11),UpperIdent(1:11-1:14),Comma(1:14-1:15),UpperIdent(1:16-1:20),CloseSquare(1:20-1:21),
LowerIdent(3:1-3:5),OpAssign(3:6-3:7),UpperIdent(3:8-3:13),NoSpaceDotUpperIdent(3:13-3:17),
EndOfFile(4:1-4:1),
~~~
# PARSE
~~~clojure
(file @1.1-3.17
	(type-module @1.1-1.6)
	(statements
		(s-type-decl @1.1-1.21
			(header @1.1-1.6 (name "Color")
				(args))
			(ty-tag-union @1.10-1.21
				(tags
					(ty @1.11-1.14 (name "Red"))
					(ty @1.16-1.20 (name "Blue")))))
		(s-decl @3.1-3.17
			(p-ident @3.1-3.5 (raw "test"))
			(e-tag @3.8-3.17 (raw "Color.Red")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @3.1-3.5 (ident "test"))
		(e-nominal @3.8-3.17 (nominal "Color")
			(e-tag @3.8-3.17 (name "Red"))))
	(s-nominal-decl @1.1-1.21
		(ty-header @1.1-1.6 (name "Color"))
		(ty-tag-union @1.10-1.21
			(ty-tag-name @1.11-1.14 (name "Red"))
			(ty-tag-name @1.16-1.20 (name "Blue")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.5 (type "Color")))
	(type_decls
		(nominal @1.1-1.21 (type "Color")
			(ty-header @1.1-1.6 (name "Color"))))
	(expressions
		(expr @3.8-3.17 (type "Color"))))
~~~

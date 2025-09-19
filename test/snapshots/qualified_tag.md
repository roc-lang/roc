# META
~~~ini
description=Simple qualified tag test
type=file
~~~
# SOURCE
~~~roc
module [Color]

Color := [Red, Blue]

test = Color.Red
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:14),CloseSquare(1:14-1:15),
UpperIdent(3:1-3:6),OpColonEqual(3:7-3:9),OpenSquare(3:10-3:11),UpperIdent(3:11-3:14),Comma(3:14-3:15),UpperIdent(3:16-3:20),CloseSquare(3:20-3:21),
LowerIdent(5:1-5:5),OpAssign(5:6-5:7),UpperIdent(5:8-5:13),NoSpaceDotUpperIdent(5:13-5:17),
EndOfFile(6:1-6:1),
~~~
# PARSE
~~~clojure
(file @1.1-5.17
	(module @1.1-1.15
		(exposes @1.8-1.15
			(exposed-upper-ident @1.9-1.14 (text "Color"))))
	(statements
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
	(s-nominal-decl @3.1-3.21
		(ty-header @3.1-3.6 (name "Color"))
		(ty-tag-union @3.10-3.21
			(tag_name @3.11-3.14 (name "Red"))
			(tag_name @3.16-3.20 (name "Blue")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @5.1-5.5 (type "Color")))
	(type_decls
		(nominal @1.1-1.1 (type "Bool")
			(ty-header @1.1-1.1 (name "Bool")))
		(nominal @1.1-1.1 (type "Result(ok, err)")
			(ty-header @1.1-1.1 (name "Result")
				(ty-args
					(ty-rigid-var @1.1-1.1 (name "ok"))
					(ty-rigid-var @1.1-1.1 (name "err")))))
		(nominal @3.1-3.21 (type "Color")
			(ty-header @3.1-3.6 (name "Color"))))
	(expressions
		(expr @5.8-5.17 (type "Color"))))
~~~

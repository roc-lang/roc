# META
~~~ini
description=Example of a simple nominal tag union
type=file:NominalTagSimple.roc
~~~
# SOURCE
~~~roc
NominalTagSimple := {}

Color := [Red, Green, Blue]

blue : Color
blue = Color.Blue

yellow : Color
yellow = Color.Yellow
~~~
# EXPECTED
INVALID NOMINAL TAG - nominal_tag_simple.md:9:10:9:22
# PROBLEMS
**INVALID NOMINAL TAG**
I'm having trouble with this nominal tag:
**nominal_tag_simple.md:9:10:9:22:**
```roc
yellow = Color.Yellow
```
         ^^^^^^^^^^^^

The tag is:
    _Yellow_

But the nominal type needs it to one of:
    _[Blue, Green, Red]_

# TOKENS
~~~zig
UpperIdent(1:1-1:17),OpColonEqual(1:18-1:20),OpenCurly(1:21-1:22),CloseCurly(1:22-1:23),
UpperIdent(3:1-3:6),OpColonEqual(3:7-3:9),OpenSquare(3:10-3:11),UpperIdent(3:11-3:14),Comma(3:14-3:15),UpperIdent(3:16-3:21),Comma(3:21-3:22),UpperIdent(3:23-3:27),CloseSquare(3:27-3:28),
LowerIdent(5:1-5:5),OpColon(5:6-5:7),UpperIdent(5:8-5:13),
LowerIdent(6:1-6:5),OpAssign(6:6-6:7),UpperIdent(6:8-6:13),NoSpaceDotUpperIdent(6:13-6:18),
LowerIdent(8:1-8:7),OpColon(8:8-8:9),UpperIdent(8:10-8:15),
LowerIdent(9:1-9:7),OpAssign(9:8-9:9),UpperIdent(9:10-9:15),NoSpaceDotUpperIdent(9:15-9:22),
EndOfFile(10:1-10:1),
~~~
# PARSE
~~~clojure
(file @1.1-9.22
	(type-module @1.1-1.17)
	(statements
		(s-type-decl @1.1-1.23
			(header @1.1-1.17 (name "NominalTagSimple")
				(args))
			(ty-record @1.21-1.23))
		(s-type-decl @3.1-3.28
			(header @3.1-3.6 (name "Color")
				(args))
			(ty-tag-union @3.10-3.28
				(tags
					(ty @3.11-3.14 (name "Red"))
					(ty @3.16-3.21 (name "Green"))
					(ty @3.23-3.27 (name "Blue")))))
		(s-type-anno @5.1-5.13 (name "blue")
			(ty @5.8-5.13 (name "Color")))
		(s-decl @6.1-6.18
			(p-ident @6.1-6.5 (raw "blue"))
			(e-tag @6.8-6.18 (raw "Color.Blue")))
		(s-type-anno @8.1-8.15 (name "yellow")
			(ty @8.10-8.15 (name "Color")))
		(s-decl @9.1-9.22
			(p-ident @9.1-9.7 (raw "yellow"))
			(e-tag @9.10-9.22 (raw "Color.Yellow")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @6.1-6.5 (ident "blue"))
		(e-nominal @6.8-6.18 (nominal "Color")
			(e-tag @6.8-6.18 (name "Blue")))
		(annotation @6.1-6.5
			(declared-type
				(ty-lookup @5.8-5.13 (name "Color") (local)))))
	(d-let
		(p-assign @9.1-9.7 (ident "yellow"))
		(e-nominal @9.10-9.22 (nominal "Color")
			(e-tag @9.10-9.22 (name "Yellow")))
		(annotation @9.1-9.7
			(declared-type
				(ty-lookup @8.10-8.15 (name "Color") (local)))))
	(s-nominal-decl @1.1-1.23
		(ty-header @1.1-1.17 (name "NominalTagSimple"))
		(ty-record @1.21-1.23))
	(s-nominal-decl @3.1-3.28
		(ty-header @3.1-3.6 (name "Color"))
		(ty-tag-union @3.10-3.28
			(ty-tag-name @3.11-3.14 (name "Red"))
			(ty-tag-name @3.16-3.21 (name "Green"))
			(ty-tag-name @3.23-3.27 (name "Blue")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.5 (type "Color"))
		(patt @9.1-9.7 (type "Error")))
	(type_decls
		(nominal @1.1-1.23 (type "NominalTagSimple")
			(ty-header @1.1-1.17 (name "NominalTagSimple")))
		(nominal @3.1-3.28 (type "Color")
			(ty-header @3.1-3.6 (name "Color"))))
	(expressions
		(expr @6.8-6.18 (type "Color"))
		(expr @9.10-9.22 (type "Error"))))
~~~

# META
~~~ini
description=Example of a simple nominal tag union
type=file:Color.roc
~~~
# SOURCE
~~~roc
Color := [Red, Green, Blue]

blue : Color
blue = Color.Blue

yellow : Color
yellow = Color.Yellow
~~~
# EXPECTED
INVALID NOMINAL TAG - nominal_tag_simple.md:7:10:7:22
# PROBLEMS
**INVALID NOMINAL TAG**
I'm having trouble with this nominal tag:
**nominal_tag_simple.md:7:10:7:22:**
```roc
yellow = Color.Yellow
```
         ^^^^^^^^^^^^

The tag is:
    _Yellow_

But it should be one of:
    _[Blue, Green, Red]_

# TOKENS
~~~zig
UpperIdent(1:1-1:6),OpColonEqual(1:7-1:9),OpenSquare(1:10-1:11),UpperIdent(1:11-1:14),Comma(1:14-1:15),UpperIdent(1:16-1:21),Comma(1:21-1:22),UpperIdent(1:23-1:27),CloseSquare(1:27-1:28),
LowerIdent(3:1-3:5),OpColon(3:6-3:7),UpperIdent(3:8-3:13),
LowerIdent(4:1-4:5),OpAssign(4:6-4:7),UpperIdent(4:8-4:13),NoSpaceDotUpperIdent(4:13-4:18),
LowerIdent(6:1-6:7),OpColon(6:8-6:9),UpperIdent(6:10-6:15),
LowerIdent(7:1-7:7),OpAssign(7:8-7:9),UpperIdent(7:10-7:15),NoSpaceDotUpperIdent(7:15-7:22),
EndOfFile(8:1-8:1),
~~~
# PARSE
~~~clojure
(file @1.1-7.22
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
		(s-type-anno @3.1-3.13 (name "blue")
			(ty @3.8-3.13 (name "Color")))
		(s-decl @4.1-4.18
			(p-ident @4.1-4.5 (raw "blue"))
			(e-tag @4.8-4.18 (raw "Color.Blue")))
		(s-type-anno @6.1-6.15 (name "yellow")
			(ty @6.10-6.15 (name "Color")))
		(s-decl @7.1-7.22
			(p-ident @7.1-7.7 (raw "yellow"))
			(e-tag @7.10-7.22 (raw "Color.Yellow")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.5 (ident "blue"))
		(e-nominal @4.8-4.18 (nominal "Color")
			(e-tag @4.8-4.18 (name "Blue")))
		(annotation @4.1-4.5
			(declared-type
				(ty @3.8-3.13 (name "Color")))))
	(d-let
		(p-assign @7.1-7.7 (ident "yellow"))
		(e-nominal @7.10-7.22 (nominal "Color")
			(e-tag @7.10-7.22 (name "Yellow")))
		(annotation @7.1-7.7
			(declared-type
				(ty @6.10-6.15 (name "Color")))))
	(s-nominal-decl @1.1-1.28
		(ty-header @1.1-1.6 (name "Color"))
		(ty-tag-union @1.10-1.28
			(ty @1.11-1.14 (name "Red"))
			(ty @1.16-1.21 (name "Green"))
			(ty @1.23-1.27 (name "Blue")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.5 (type "Error"))
		(patt @7.1-7.7 (type "Error")))
	(type_decls
		(nominal @1.1-1.28 (type "Error")
			(ty-header @1.1-1.6 (name "Color"))))
	(expressions
		(expr @4.8-4.18 (type "Error"))
		(expr @7.10-7.22 (type "Error"))))
~~~

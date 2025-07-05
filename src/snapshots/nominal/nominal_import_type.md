# META
~~~ini
description=Example of importing a nominal tag union from another module
type=file
~~~
# SOURCE
~~~roc
module [red]

import Color

red : Color.RGB
red = Color.RGB.Red
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:12),CloseSquare(1:12-1:13),Newline(1:1-1:1),
Newline(1:1-1:1),
KwImport(3:1-3:7),UpperIdent(3:8-3:13),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(5:1-5:4),OpColon(5:5-5:6),UpperIdent(5:7-5:12),NoSpaceDotUpperIdent(5:12-5:16),Newline(1:1-1:1),
LowerIdent(6:1-6:4),OpAssign(6:5-6:6),UpperIdent(6:7-6:12),NoSpaceDotUpperIdent(6:12-6:16),NoSpaceDotUpperIdent(6:16-6:20),EndOfFile(6:20-6:20),
~~~
# PARSE
~~~clojure
(file @1.1-6.20
	(module @1.1-1.13
		(exposes @1.8-1.13
			(exposed-lower-ident (text "red"))))
	(statements
		(s-import @3.1-3.13 (raw "Color"))
		(s-type-anno @1.1-1.1 (name "red")
			(ty @5.7-5.16 (name "Color.RGB")))
		(s-decl @6.1-6.20
			(p-ident @6.1-6.4 (raw "red"))
			(e-tag @6.7-6.20 (raw "Color.RGB.Red")))))
~~~
# FORMATTED
~~~roc
module [red]

import Color

red : Color.RGB
red = Red
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @6.1-6.4 (ident "red"))
		(e-tag @6.7-6.20 (name "Red"))
		(annotation @6.1-6.4
			(declared-type
				(ty-lookup-external @5.7-5.16
					(ext-decl @5.7-5.16 (ident "Color.RGB") (kind "type"))))))
	(s-import @3.1-3.13 (module "Color")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.4 (type "[Red]*")))
	(expressions
		(expr @6.7-6.20 (type "[Red]*"))))
~~~

# META
~~~ini
description=Example of importing a nominal tag union from another module
type=snippet
~~~
# SOURCE
~~~roc
import Color

red : Color.RGB
red = Color.RGB.Red
~~~
# EXPECTED
MODULE NOT FOUND - nominal_import_type.md:1:1:1:13
# PROBLEMS
**MODULE NOT FOUND**
The module `Color` was not found in this Roc project.

You're attempting to use this module here:
**nominal_import_type.md:1:1:1:13:**
```roc
import Color
```
^^^^^^^^^^^^


# TOKENS
~~~zig
KwImport(1:1-1:7),UpperIdent(1:8-1:13),
LowerIdent(3:1-3:4),OpColon(3:5-3:6),UpperIdent(3:7-3:12),NoSpaceDotUpperIdent(3:12-3:16),
LowerIdent(4:1-4:4),OpAssign(4:5-4:6),UpperIdent(4:7-4:12),NoSpaceDotUpperIdent(4:12-4:16),NoSpaceDotUpperIdent(4:16-4:20),
EndOfFile(5:1-5:1),
~~~
# PARSE
~~~clojure
(file @1.1-4.20
	(type-module @1.1-1.7)
	(statements
		(s-import @1.1-1.13 (raw "Color"))
		(s-type-anno @3.1-3.16 (name "red")
			(ty @3.7-3.16 (name "Color.RGB")))
		(s-decl @4.1-4.20
			(p-ident @4.1-4.4 (raw "red"))
			(e-tag @4.7-4.20 (raw "Color.RGB.Red")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.4 (ident "red"))
		(e-nominal-external @4.7-4.20
			(module-idx "2")
			(target-node-idx "0")
			(e-tag @4.7-4.20 (name "Red")))
		(annotation @4.1-4.4
			(declared-type
				(ty-lookup @3.7-3.16 (name "RGB") (external (module-idx "2") (target-node-idx "0"))))))
	(s-import @1.1-1.13 (module "Color")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.4 (type "Error")))
	(expressions
		(expr @4.7-4.20 (type "Error"))))
~~~

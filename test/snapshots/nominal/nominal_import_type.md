# META
~~~ini
description=Example of importing a nominal tag union from another module
type=file:NominalImportType.roc
~~~
# SOURCE
~~~roc
NominalImportType := {}

import Color

red : Color.RGB
red = Color.RGB.Red
~~~
# EXPECTED
MODULE NOT FOUND - nominal_import_type.md:3:1:3:13
# PROBLEMS
**MODULE NOT FOUND**
The module `Color` was not found in this Roc project.

You're attempting to use this module here:
**nominal_import_type.md:3:1:3:13:**
```roc
import Color
```
^^^^^^^^^^^^


# TOKENS
~~~zig
UpperIdent(1:1-1:18),OpColonEqual(1:19-1:21),OpenCurly(1:22-1:23),CloseCurly(1:23-1:24),
KwImport(3:1-3:7),UpperIdent(3:8-3:13),
LowerIdent(5:1-5:4),OpColon(5:5-5:6),UpperIdent(5:7-5:12),NoSpaceDotUpperIdent(5:12-5:16),
LowerIdent(6:1-6:4),OpAssign(6:5-6:6),UpperIdent(6:7-6:12),NoSpaceDotUpperIdent(6:12-6:16),NoSpaceDotUpperIdent(6:16-6:20),
EndOfFile(7:1-7:1),
~~~
# PARSE
~~~clojure
(file @1.1-6.20
	(type-module @1.1-1.18)
	(statements
		(s-type-decl @1.1-1.24
			(header @1.1-1.18 (name "NominalImportType")
				(args))
			(ty-record @1.22-1.24))
		(s-import @3.1-3.13 (raw "Color"))
		(s-type-anno @5.1-5.16 (name "red")
			(ty @5.7-5.16 (name "Color.RGB")))
		(s-decl @6.1-6.20
			(p-ident @6.1-6.4 (raw "red"))
			(e-tag @6.7-6.20 (raw "Color.RGB.Red")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @6.1-6.4 (ident "red"))
		(e-nominal-external @6.7-6.20
			(module-idx "0")
			(target-node-idx "0")
			(e-tag @6.7-6.20 (name "Red")))
		(annotation @6.1-6.4
			(declared-type
				(ty-lookup @5.7-5.16 (name "RGB") (external (module-idx "0") (target-node-idx "0"))))))
	(s-nominal-decl @1.1-1.24
		(ty-header @1.1-1.18 (name "NominalImportType"))
		(ty-record @1.22-1.24))
	(s-import @3.1-3.13 (module "Color")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.4 (type "Error")))
	(type_decls
		(nominal @1.1-1.24 (type "NominalImportType")
			(ty-header @1.1-1.18 (name "NominalImportType"))))
	(expressions
		(expr @6.7-6.20 (type "Error"))))
~~~

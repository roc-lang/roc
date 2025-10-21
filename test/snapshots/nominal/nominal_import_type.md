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
KwImport,UpperIdent,
LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-import (raw "Color"))
		(s-type-anno (name "red")
			(ty (name "Color.RGB")))
		(s-decl
			(p-ident (raw "red"))
			(e-tag (raw "Color.RGB.Red")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "red"))
		(e-nominal-external
			(module "Color")
			(e-tag (name "Red")))
		(annotation
			(declared-type
				(ty-lookup (name "RGB") (module "Color")))))
	(s-import (module "Color")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~

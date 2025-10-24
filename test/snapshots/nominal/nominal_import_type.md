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
TYPE NOT EXPOSED - nominal_import_type.md:3:12:3:16
UNDECLARED TYPE - nominal_import_type.md:4:12:4:16
# PROBLEMS
**MODULE NOT FOUND**
The module `Color` was not found in this Roc project.

You're attempting to use this module here:
**nominal_import_type.md:1:1:1:13:**
```roc
import Color
```
^^^^^^^^^^^^


**TYPE NOT EXPOSED**
The type `RGB` is not an exposed by the module `Color`.

You're attempting to use this type here:
**nominal_import_type.md:3:12:3:16:**
```roc
red : Color.RGB
```
           ^^^^


**UNDECLARED TYPE**
The type _Color.RGB_ is not declared in this scope.

This type is referenced here:
**nominal_import_type.md:4:12:4:16:**
```roc
red = Color.RGB.Red
```
           ^^^^


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
		(e-runtime-error (tag "undeclared_type"))
		(annotation
			(ty-malformed)))
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

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
MODULE NOT FOUND - nominal_import_type.md:3:12:3:16
MODULE NOT FOUND - nominal_import_type.md:4:12:4:16
# PROBLEMS

┌──────────────────┐
│ MODULE NOT FOUND ├─ This `RGB` type is declared to be in `Color`, which ────┐
└┬─────────────────┘  does not exist.                                         │
 │                                                                            │
 │  red : Color.RGB                                                           │
 │             ‾‾‾‾                                                           │
 └─────────────────────────────────────────────── nominal_import_type.md:3:12 ┘



┌──────────────────┐
│ MODULE NOT FOUND ├─ This `RGB` type is declared to be in `Color`, which ────┐
└┬─────────────────┘  does not exist.                                         │
 │                                                                            │
 │  red = Color.RGB.Red                                                       │
 │             ‾‾‾‾                                                           │
 └─────────────────────────────────────────────── nominal_import_type.md:4:12 ┘


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
		(e-runtime-error (tag "type_from_missing_module"))
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

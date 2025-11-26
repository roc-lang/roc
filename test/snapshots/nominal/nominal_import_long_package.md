# META
~~~ini
description=Example of importing a nominal tag union from a module within a package, and renaming it using `as`
type=snippet
~~~
# SOURCE
~~~roc
import design.Styles.Color exposing [Encoder as CE]

red : CE
red = ... # not implemented
~~~
# EXPECTED
MODULE NOT FOUND - nominal_import_long_package.md:1:1:1:52
UNDECLARED TYPE - nominal_import_long_package.md:3:7:3:9
# PROBLEMS
**MODULE NOT FOUND**
The module `design.Styles.Color` was not found in this Roc project.

You're attempting to use this module here:
**nominal_import_long_package.md:1:1:1:52:**
```roc
import design.Styles.Color exposing [Encoder as CE]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNDECLARED TYPE**
The type _CE_ is not declared in this scope.

This type is referenced here:
**nominal_import_long_package.md:3:7:3:9:**
```roc
red : CE
```
      ^^


# TOKENS
~~~zig
KwImport,LowerIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,KwExposing,OpenSquare,UpperIdent,KwAs,UpperIdent,CloseSquare,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,TripleDot,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-import (raw "design.Color")
			(exposing
				(exposed-upper-ident (text "Encoder") (as "CE"))))
		(s-type-anno (name "red")
			(ty (name "CE")))
		(s-decl
			(p-ident (raw "red"))
			(e-ellipsis))))
~~~
# FORMATTED
~~~roc
import design.Color exposing [Encoder as CE]

red : CE
red = ... # not implemented
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "red"))
		(e-not-implemented)
		(annotation
			(ty-malformed)))
	(s-import (module "design.Styles.Color")
		(exposes
			(exposed (name "Encoder") (alias "CE") (wildcard false)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~

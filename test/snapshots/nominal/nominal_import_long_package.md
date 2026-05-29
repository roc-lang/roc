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
UNDECLARED TYPE - nominal_import_long_package.md:3:7:3:9
NOT IMPLEMENTED - nominal_import_long_package.md:1:1:1:1
# PROBLEMS
**UNDECLARED TYPE**
The type _CE_ is not declared in this scope.

This type is referenced here:
**nominal_import_long_package.md:3:7:3:9:**
```roc
red : CE
```
      ^^


**NOT IMPLEMENTED**
This feature is not yet implemented: ellipsis expression

**nominal_import_long_package.md:1:1:1:1:**
```roc
import design.Styles.Color exposing [Encoder as CE]
```
^

This error doesn't have a proper diagnostic report yet. Let us know if you want to help improve Roc's error messages!


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
		(s-import (raw "design.Styles.Color")
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
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "red"))
		(e-runtime-error (tag "not_implemented"))
		(annotation
			(ty-malformed)))
	(s-import (module "design.Styles")
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

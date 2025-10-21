# META
~~~ini
description=Example of importing constructors with wildcard from a nominal tag union
type=snippet
~~~
# SOURCE
~~~roc
import Color.*

red : Color
red = Red

blue : Color
blue = Blue

green : Color
green = Green
~~~
# EXPECTED
PARSE ERROR - nominal_import_wildcard.md:1:13:1:15
MODULE NOT FOUND - nominal_import_wildcard.md:1:1:1:13
UNDECLARED TYPE - nominal_import_wildcard.md:3:7:3:12
UNDECLARED TYPE - nominal_import_wildcard.md:6:8:6:13
UNDECLARED TYPE - nominal_import_wildcard.md:9:9:9:14
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**nominal_import_wildcard.md:1:13:1:15:**
```roc
import Color.*
```
            ^^


**MODULE NOT FOUND**
The module `Color` was not found in this Roc project.

You're attempting to use this module here:
**nominal_import_wildcard.md:1:1:1:13:**
```roc
import Color.*
```
^^^^^^^^^^^^


**UNDECLARED TYPE**
The type _Color_ is not declared in this scope.

This type is referenced here:
**nominal_import_wildcard.md:3:7:3:12:**
```roc
red : Color
```
      ^^^^^


**UNDECLARED TYPE**
The type _Color_ is not declared in this scope.

This type is referenced here:
**nominal_import_wildcard.md:6:8:6:13:**
```roc
blue : Color
```
       ^^^^^


**UNDECLARED TYPE**
The type _Color_ is not declared in this scope.

This type is referenced here:
**nominal_import_wildcard.md:9:9:9:14:**
```roc
green : Color
```
        ^^^^^


# TOKENS
~~~zig
KwImport,UpperIdent,DotStar,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-import (raw "Color"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-type-anno (name "red")
			(ty (name "Color")))
		(s-decl
			(p-ident (raw "red"))
			(e-tag (raw "Red")))
		(s-type-anno (name "blue")
			(ty (name "Color")))
		(s-decl
			(p-ident (raw "blue"))
			(e-tag (raw "Blue")))
		(s-type-anno (name "green")
			(ty (name "Color")))
		(s-decl
			(p-ident (raw "green"))
			(e-tag (raw "Green")))))
~~~
# FORMATTED
~~~roc
import Color


red : Color
red = Red

blue : Color
blue = Blue

green : Color
green = Green
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "red"))
		(e-tag (name "Red"))
		(annotation
			(ty-malformed)))
	(d-let
		(p-assign (ident "blue"))
		(e-tag (name "Blue"))
		(annotation
			(ty-malformed)))
	(d-let
		(p-assign (ident "green"))
		(e-tag (name "Green"))
		(annotation
			(ty-malformed)))
	(s-import (module "Color")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Error"))))
~~~

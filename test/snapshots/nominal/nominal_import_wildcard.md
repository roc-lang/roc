# META
~~~ini
description=Example of importing constructors with wildcard from a nominal tag union
type=file
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
MISSING MAIN! FUNCTION - nominal_import_wildcard.md:1:1:10:14
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


**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**nominal_import_wildcard.md:1:1:10:14:**
```roc
import Color.*

red : Color
red = Red

blue : Color
blue = Blue

green : Color
green = Green
```


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
KwImport(1:1-1:7),UpperIdent(1:8-1:13),DotStar(1:13-1:15),
LowerIdent(3:1-3:4),OpColon(3:5-3:6),UpperIdent(3:7-3:12),
LowerIdent(4:1-4:4),OpAssign(4:5-4:6),UpperIdent(4:7-4:10),
LowerIdent(6:1-6:5),OpColon(6:6-6:7),UpperIdent(6:8-6:13),
LowerIdent(7:1-7:5),OpAssign(7:6-7:7),UpperIdent(7:8-7:12),
LowerIdent(9:1-9:6),OpColon(9:7-9:8),UpperIdent(9:9-9:14),
LowerIdent(10:1-10:6),OpAssign(10:7-10:8),UpperIdent(10:9-10:14),
EndOfFile(11:1-11:1),
~~~
# PARSE
~~~clojure
(file @1.1-10.14
	(type-module @1.1-1.7)
	(statements
		(s-import @1.1-1.13 (raw "Color"))
		(s-malformed @1.13-1.15 (tag "statement_unexpected_token"))
		(s-type-anno @3.1-3.12 (name "red")
			(ty @3.7-3.12 (name "Color")))
		(s-decl @4.1-4.10
			(p-ident @4.1-4.4 (raw "red"))
			(e-tag @4.7-4.10 (raw "Red")))
		(s-type-anno @6.1-6.13 (name "blue")
			(ty @6.8-6.13 (name "Color")))
		(s-decl @7.1-7.12
			(p-ident @7.1-7.5 (raw "blue"))
			(e-tag @7.8-7.12 (raw "Blue")))
		(s-type-anno @9.1-9.14 (name "green")
			(ty @9.9-9.14 (name "Color")))
		(s-decl @10.1-10.14
			(p-ident @10.1-10.6 (raw "green"))
			(e-tag @10.9-10.14 (raw "Green")))))
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
		(p-assign @4.1-4.4 (ident "red"))
		(e-tag @4.7-4.10 (name "Red"))
		(annotation @4.1-4.4
			(declared-type
				(ty @3.7-3.12 (name "Color")))))
	(d-let
		(p-assign @7.1-7.5 (ident "blue"))
		(e-tag @7.8-7.12 (name "Blue"))
		(annotation @7.1-7.5
			(declared-type
				(ty @6.8-6.13 (name "Color")))))
	(d-let
		(p-assign @10.1-10.6 (ident "green"))
		(e-tag @10.9-10.14 (name "Green"))
		(annotation @10.1-10.6
			(declared-type
				(ty @9.9-9.14 (name "Color")))))
	(s-import @1.1-1.13 (module "Color")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.4 (type "Error"))
		(patt @7.1-7.5 (type "Error"))
		(patt @10.1-10.6 (type "Error")))
	(expressions
		(expr @4.7-4.10 (type "Error"))
		(expr @7.8-7.12 (type "Error"))
		(expr @10.9-10.14 (type "Error"))))
~~~

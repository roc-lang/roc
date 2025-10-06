# META
~~~ini
description=Example of importing a nominal tag union from a module within a package, and renaming it using `as`
type=file:NominalImportLongPackage.roc
~~~
# SOURCE
~~~roc
NominalImportLongPackage := {}

import design.Styles.Color exposing [Encoder as CE]

red : CE
red = ... # not implemented
~~~
# EXPECTED
PARSE ERROR - nominal_import_long_package.md:3:21:3:27
PARSE ERROR - nominal_import_long_package.md:3:28:3:36
PARSE ERROR - nominal_import_long_package.md:3:37:3:38
PARSE ERROR - nominal_import_long_package.md:3:46:3:48
PARSE ERROR - nominal_import_long_package.md:3:51:3:52
MODULE NOT FOUND - nominal_import_long_package.md:3:1:3:21
UNDECLARED TYPE - nominal_import_long_package.md:5:7:5:9
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**nominal_import_long_package.md:3:21:3:27:**
```roc
import design.Styles.Color exposing [Encoder as CE]
```
                    ^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**nominal_import_long_package.md:3:28:3:36:**
```roc
import design.Styles.Color exposing [Encoder as CE]
```
                           ^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**nominal_import_long_package.md:3:37:3:38:**
```roc
import design.Styles.Color exposing [Encoder as CE]
```
                                    ^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Result(a, Str)`
    `Maybe(List(U64))`

**nominal_import_long_package.md:3:46:3:48:**
```roc
import design.Styles.Color exposing [Encoder as CE]
```
                                             ^^


**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Result(a, Str)`
    `Maybe(List(U64))`

**nominal_import_long_package.md:3:51:3:52:**
```roc
import design.Styles.Color exposing [Encoder as CE]
```
                                                  ^


**MODULE NOT FOUND**
The module `design.Styles` was not found in this Roc project.

You're attempting to use this module here:
**nominal_import_long_package.md:3:1:3:21:**
```roc
import design.Styles.Color exposing [Encoder as CE]
```
^^^^^^^^^^^^^^^^^^^^


**UNDECLARED TYPE**
The type _CE_ is not declared in this scope.

This type is referenced here:
**nominal_import_long_package.md:5:7:5:9:**
```roc
red : CE
```
      ^^


# TOKENS
~~~zig
UpperIdent(1:1-1:25),OpColonEqual(1:26-1:28),OpenCurly(1:29-1:30),CloseCurly(1:30-1:31),
KwImport(3:1-3:7),LowerIdent(3:8-3:14),NoSpaceDotUpperIdent(3:14-3:21),NoSpaceDotUpperIdent(3:21-3:27),KwExposing(3:28-3:36),OpenSquare(3:37-3:38),UpperIdent(3:38-3:45),KwAs(3:46-3:48),UpperIdent(3:49-3:51),CloseSquare(3:51-3:52),
LowerIdent(5:1-5:4),OpColon(5:5-5:6),UpperIdent(5:7-5:9),
LowerIdent(6:1-6:4),OpAssign(6:5-6:6),TripleDot(6:7-6:10),
EndOfFile(7:1-7:1),
~~~
# PARSE
~~~clojure
(file @1.1-6.10
	(type-module @1.1-1.25)
	(statements
		(s-type-decl @1.1-1.31
			(header @1.1-1.25 (name "NominalImportLongPackage")
				(args))
			(ty-record @1.29-1.31))
		(s-import @3.1-3.21 (raw "design.Styles"))
		(s-malformed @3.21-3.27 (tag "statement_unexpected_token"))
		(s-malformed @3.28-3.36 (tag "statement_unexpected_token"))
		(s-malformed @3.37-3.38 (tag "statement_unexpected_token"))
		(s-malformed @3.46-3.48 (tag "expected_colon_after_type_annotation"))
		(s-malformed @3.51-3.52 (tag "expected_colon_after_type_annotation"))
		(s-type-anno @5.1-5.9 (name "red")
			(ty @5.7-5.9 (name "CE")))
		(s-decl @6.1-6.10
			(p-ident @6.1-6.4 (raw "red"))
			(e-ellipsis))))
~~~
# FORMATTED
~~~roc
NominalImportLongPackage := {}

import design.Styles


red : CE
red = ... # not implemented
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @6.1-6.4 (ident "red"))
		(e-not-implemented @1.1-1.1)
		(annotation @6.1-6.4
			(declared-type
				(ty-malformed @5.7-5.9))))
	(s-nominal-decl @1.1-1.31
		(ty-header @1.1-1.25 (name "NominalImportLongPackage"))
		(ty-record @1.29-1.31))
	(s-import @3.1-3.21 (module "design.Styles") (qualifier "design")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.4 (type "Error")))
	(type_decls
		(nominal @1.1-1.31 (type "NominalImportLongPackage")
			(ty-header @1.1-1.25 (name "NominalImportLongPackage"))))
	(expressions
		(expr @1.1-1.1 (type "Error"))))
~~~

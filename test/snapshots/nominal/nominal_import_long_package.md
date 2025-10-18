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
PARSE ERROR - nominal_import_long_package.md:1:21:1:27
PARSE ERROR - nominal_import_long_package.md:1:28:1:36
PARSE ERROR - nominal_import_long_package.md:1:37:1:38
PARSE ERROR - nominal_import_long_package.md:1:46:1:48
PARSE ERROR - nominal_import_long_package.md:1:51:1:52
MODULE NOT FOUND - nominal_import_long_package.md:1:1:1:21
UNDECLARED TYPE - nominal_import_long_package.md:3:7:3:9
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**nominal_import_long_package.md:1:21:1:27:**
```roc
import design.Styles.Color exposing [Encoder as CE]
```
                    ^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**nominal_import_long_package.md:1:28:1:36:**
```roc
import design.Styles.Color exposing [Encoder as CE]
```
                           ^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**nominal_import_long_package.md:1:37:1:38:**
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

**nominal_import_long_package.md:1:46:1:48:**
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

**nominal_import_long_package.md:1:51:1:52:**
```roc
import design.Styles.Color exposing [Encoder as CE]
```
                                                  ^


**MODULE NOT FOUND**
The module `design.Styles` was not found in this Roc project.

You're attempting to use this module here:
**nominal_import_long_package.md:1:1:1:21:**
```roc
import design.Styles.Color exposing [Encoder as CE]
```
^^^^^^^^^^^^^^^^^^^^


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
		(s-import (raw "design.Styles"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-malformed (tag "expected_colon_after_type_annotation"))
		(s-type-anno (name "red")
			(ty (name "CE")))
		(s-decl
			(p-ident (raw "red"))
			(e-ellipsis))))
~~~
# FORMATTED
~~~roc
import design.Styles


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
			(declared-type
				(ty-malformed))))
	(s-import (module "design.Styles")
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

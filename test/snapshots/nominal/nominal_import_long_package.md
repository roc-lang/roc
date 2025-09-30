# META
~~~ini
description=Example of importing a nominal tag union from a module within a package, and renaming it using `as`
type=file
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
MISSING MAIN! FUNCTION - nominal_import_long_package.md:1:1:4:10
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


**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**nominal_import_long_package.md:1:1:4:10:**
```roc
import design.Styles.Color exposing [Encoder as CE]

red : CE
red = ... # not implemented
```


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
KwImport(1:1-1:7),LowerIdent(1:8-1:14),NoSpaceDotUpperIdent(1:14-1:21),NoSpaceDotUpperIdent(1:21-1:27),KwExposing(1:28-1:36),OpenSquare(1:37-1:38),UpperIdent(1:38-1:45),KwAs(1:46-1:48),UpperIdent(1:49-1:51),CloseSquare(1:51-1:52),
LowerIdent(3:1-3:4),OpColon(3:5-3:6),UpperIdent(3:7-3:9),
LowerIdent(4:1-4:4),OpAssign(4:5-4:6),TripleDot(4:7-4:10),
EndOfFile(5:1-5:1),
~~~
# PARSE
~~~clojure
(file @1.1-4.10
	(type-module @1.1-1.7)
	(statements
		(s-import @1.1-1.21 (raw "design.Styles"))
		(s-malformed @1.21-1.27 (tag "statement_unexpected_token"))
		(s-malformed @1.28-1.36 (tag "statement_unexpected_token"))
		(s-malformed @1.37-1.38 (tag "statement_unexpected_token"))
		(s-malformed @1.46-1.48 (tag "expected_colon_after_type_annotation"))
		(s-malformed @1.51-1.52 (tag "expected_colon_after_type_annotation"))
		(s-type-anno @3.1-3.9 (name "red")
			(ty @3.7-3.9 (name "CE")))
		(s-decl @4.1-4.10
			(p-ident @4.1-4.4 (raw "red"))
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
		(p-assign @4.1-4.4 (ident "red"))
		(e-not-implemented @1.1-1.1)
		(annotation @4.1-4.4
			(declared-type
				(ty @3.7-3.9 (name "CE")))))
	(s-import @1.1-1.21 (module "design.Styles") (qualifier "design")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.4 (type "Error")))
	(expressions
		(expr @1.1-1.1 (type "Error"))))
~~~

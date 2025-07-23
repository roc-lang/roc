# META
~~~ini
description=Example of importing constructors with wildcard from a nominal tag union
type=file
~~~
# SOURCE
~~~roc
module [red, green, blue]

import Color.*

red : Color
red = Red

blue : Color
blue = Blue

green : Color
green = Green
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - nominal_import_wildcard.md:3:13:3:15
COMPILER DIAGNOSTIC - nominal_import_wildcard.md:0:0:0:0
COMPILER DIAGNOSTIC - nominal_import_wildcard.md:0:0:0:0
COMPILER DIAGNOSTIC - nominal_import_wildcard.md:0:0:0:0
COMPILER DIAGNOSTIC - nominal_import_wildcard.md:0:0:0:0
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **.*** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**nominal_import_wildcard.md:3:13:3:15:**
```roc
import Color.*
```
            ^^


**COMPILER DIAGNOSTIC**

**Compiler Diagnostic**
Diagnostic type 'invalid_top_level_statement' is not yet handled in report generation.
**nominal_import_wildcard.md:0:0:0:0**

**COMPILER DIAGNOSTIC**

**Compiler Diagnostic**
Diagnostic type 'undeclared_type' is not yet handled in report generation.
**nominal_import_wildcard.md:0:0:0:0**

**COMPILER DIAGNOSTIC**

**Compiler Diagnostic**
Diagnostic type 'undeclared_type' is not yet handled in report generation.
**nominal_import_wildcard.md:0:0:0:0**

**COMPILER DIAGNOSTIC**

**Compiler Diagnostic**
Diagnostic type 'undeclared_type' is not yet handled in report generation.
**nominal_import_wildcard.md:0:0:0:0**

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:12),Comma(1:12-1:13),LowerIdent(1:14-1:19),Comma(1:19-1:20),LowerIdent(1:21-1:25),CloseSquare(1:25-1:26),
KwImport(3:1-3:7),UpperIdent(3:8-3:13),DotStar(3:13-3:15),
LowerIdent(5:1-5:4),OpColon(5:5-5:6),UpperIdent(5:7-5:12),
LowerIdent(6:1-6:4),OpAssign(6:5-6:6),UpperIdent(6:7-6:10),
LowerIdent(8:1-8:5),OpColon(8:6-8:7),UpperIdent(8:8-8:13),
LowerIdent(9:1-9:5),OpAssign(9:6-9:7),UpperIdent(9:8-9:12),
LowerIdent(11:1-11:6),OpColon(11:7-11:8),UpperIdent(11:9-11:14),
LowerIdent(12:1-12:6),OpAssign(12:7-12:8),UpperIdent(12:9-12:14),EndOfFile(12:14-12:14),
~~~
# PARSE
~~~clojure
(file @1.1-12.14
	(module @1.1-1.26
		(exposes @1.8-1.26
			(exposed-lower-ident @1.9-1.12
				(text "red"))
			(exposed-lower-ident @1.14-1.19
				(text "green"))
			(exposed-lower-ident @1.21-1.25
				(text "blue"))))
	(statements
		(s-import @3.1-3.13 (raw "Color"))
		(e-malformed @3.13-3.15 (reason "expr_unexpected_token"))
		(s-type-anno @5.1-5.12 (name "red")
			(ty @5.7-5.12 (name "Color")))
		(s-decl @6.1-6.10
			(p-ident @6.1-6.4 (raw "red"))
			(e-tag @6.7-6.10 (raw "Red")))
		(s-type-anno @8.1-8.13 (name "blue")
			(ty @8.8-8.13 (name "Color")))
		(s-decl @9.1-9.12
			(p-ident @9.1-9.5 (raw "blue"))
			(e-tag @9.8-9.12 (raw "Blue")))
		(s-type-anno @11.1-11.14 (name "green")
			(ty @11.9-11.14 (name "Color")))
		(s-decl @12.1-12.14
			(p-ident @12.1-12.6 (raw "green"))
			(e-tag @12.9-12.14 (raw "Green")))))
~~~
# FORMATTED
~~~roc
module [red, green, blue]

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
	(def
		(pattern
			(p-assign @6.1-6.4 (ident "red")))
		(expr
			(e-tag @6.7-6.10 (name "Red")))
		(annotation
			(annotation
				(type-anno
					(ty @5.7-5.12 (name "Color"))))))
	(def
		(pattern
			(p-assign @9.1-9.5 (ident "blue")))
		(expr
			(e-tag @9.8-9.12 (name "Blue")))
		(annotation
			(annotation
				(type-anno
					(ty @8.8-8.13 (name "Color"))))))
	(def
		(pattern
			(p-assign @12.1-12.6 (ident "green")))
		(expr
			(e-tag @12.9-12.14 (name "Green")))
		(annotation
			(annotation
				(type-anno
					(ty @11.9-11.14 (name "Color"))))))
	(s-import @3.1-3.13 (module "Color")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.4 (type "Error"))
		(patt @9.1-9.5 (type "Error"))
		(patt @12.1-12.6 (type "Error")))
	(expressions
		(expr @6.7-6.10 (type "Error"))
		(expr @9.8-9.12 (type "Error"))
		(expr @12.9-12.14 (type "Error"))))
~~~

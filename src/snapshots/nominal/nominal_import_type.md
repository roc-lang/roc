# META
~~~ini
description=Example of importing a nominal tag union from another module
type=file
~~~
# SOURCE
~~~roc
module [red]

import Color

red : Color.RGB
red = Color.RGB.Red
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - nominal_import_type.md:7:1:7:3
UNEXPECTED TOKEN IN EXPRESSION - nominal_import_type.md:7:2:7:4
UNEXPECTED TOKEN IN EXPRESSION - nominal_import_type.md:7:3:7:4
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**nominal_import_type.md:7:1:7:3:**
```roc
~~~
```
^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**nominal_import_type.md:7:2:7:4:**
```roc
~~~
```
 ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**nominal_import_type.md:7:3:7:4:**
```roc
~~~
```
  ^


**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expression** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:12),CloseSquare(1:12-1:13),Newline(1:1-1:1),
Newline(1:1-1:1),
KwImport(3:1-3:7),UpperIdent(3:8-3:13),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(5:1-5:4),OpColon(5:5-5:6),UpperIdent(5:7-5:12),NoSpaceDotUpperIdent(5:12-5:16),Newline(1:1-1:1),
LowerIdent(6:1-6:4),OpAssign(6:5-6:6),UpperIdent(6:7-6:12),NoSpaceDotUpperIdent(6:12-6:16),NoSpaceDotUpperIdent(6:16-6:20),Newline(1:1-1:1),
MalformedUnknownToken(7:1-7:2),MalformedUnknownToken(7:2-7:3),MalformedUnknownToken(7:3-7:4),EndOfFile(7:4-7:4),
~~~
# PARSE
~~~clojure
(file @1.1-7.4
	(module @1.1-1.13
		(exposes @1.8-1.13
			(exposed-lower-ident (text "red"))))
	(statements
		(s-import @3.1-3.13 (raw "Color"))
		(s-type-anno @1.1-1.1 (name "red")
			(ty @5.7-5.16 (name "Color.RGB")))
		(s-decl @6.1-6.20
			(p-ident @6.1-6.4 (raw "red"))
			(e-tag @6.7-6.20 (raw "Color.RGB.Red")))
		(e-malformed @7.1-7.3 (reason "expr_unexpected_token"))
		(e-malformed @7.2-7.4 (reason "expr_unexpected_token"))
		(e-malformed @7.3-7.4 (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc
module [red]

import Color

red : Color.RGB
red = Red

~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @6.1-6.4 (ident "red"))
		(e-tag @6.7-6.20 (name "Red"))
		(annotation @6.1-6.4
			(declared-type
				(ty-lookup-external @5.7-5.16
					(ext-decl @5.7-5.16 (ident "Color.RGB") (kind "type"))))))
	(s-import @3.1-3.13 (module "Color")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.4 (type "[Red]*")))
	(expressions
		(expr @6.7-6.20 (type "[Red]*"))))
~~~

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
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **.RGB.Red** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**nominal_import_type.md:6:12:6:20:**
```roc
red = Color.RGB.Red
```
           ^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **.Red** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**nominal_import_type.md:6:16:6:20:**
```roc
red = Color.RGB.Red
```
               ^^^^


**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:12),CloseSquare(1:12-1:13),Newline(1:1-1:1),
Newline(1:1-1:1),
KwImport(3:1-3:7),UpperIdent(3:8-3:13),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(5:1-5:4),OpColon(5:5-5:6),UpperIdent(5:7-5:12),NoSpaceDotUpperIdent(5:12-5:16),Newline(1:1-1:1),
LowerIdent(6:1-6:4),OpAssign(6:5-6:6),UpperIdent(6:7-6:12),NoSpaceDotUpperIdent(6:12-6:16),NoSpaceDotUpperIdent(6:16-6:20),EndOfFile(6:20-6:20),
~~~
# PARSE
~~~clojure
(file @1.1-6.20
	(module @1.1-1.13
		(exposes @1.8-1.13
			(exposed-lower-ident (text "red"))))
	(statements
		(s-import @3.1-3.13 (module "Color"))
		(s-type-anno @5.1-6.4 (name "red")
			(ty-mod (module "RGB") (name "Color")))
		(s-decl @6.1-6.12
			(p-ident @6.1-6.4 (raw "red"))
			(e-tag @6.7-6.12 (raw "Color")))
		(e-malformed @6.12-6.20 (reason "expr_unexpected_token"))
		(e-malformed @6.16-6.20 (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc
module [red]

import Color

red : Color.RGB
red = Color
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @6.1-6.4 (ident "red"))
		(e-tag @6.7-6.12 (name "Color") (args "TODO"))
		(annotation @6.1-6.4
			(declared-type
				(ty-mod @5.7-5.16 (module "RGB") (type "Color")))))
	(s-import @3.1-3.13 (module "Color")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.4 (type "[Color]a")))
	(expressions
		(expr @6.7-6.12 (type "[Color]a"))))
~~~

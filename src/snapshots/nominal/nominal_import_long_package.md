# META
~~~ini
description=Example of importing a nominal tag union from a module within a package, and renaming it using `as`
type=file
~~~
# SOURCE
~~~roc
module [red]

import design.Styles.Color exposing [Encoder as CE]

red : CE
red = ... # not implemented
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - nominal_import_long_package.md:3:21:3:36
UNEXPECTED TOKEN IN EXPRESSION - nominal_import_long_package.md:3:28:3:38
LIST NOT CLOSED - nominal_import_long_package.md:3:51:3:51
UNEXPECTED TOKEN IN EXPRESSION - nominal_import_long_package.md:7:1:7:3
UNEXPECTED TOKEN IN EXPRESSION - nominal_import_long_package.md:7:2:7:4
UNEXPECTED TOKEN IN EXPRESSION - nominal_import_long_package.md:7:3:7:4
INVALID STATEMENT - nominal_import_long_package.md:5:7:5:9
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **.Color exposing** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**nominal_import_long_package.md:3:21:3:36:**
```roc
import design.Styles.Color exposing [Encoder as CE]
```
                    ^^^^^^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **exposing [** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**nominal_import_long_package.md:3:28:3:38:**
```roc
import design.Styles.Color exposing [Encoder as CE]
```
                           ^^^^^^^^^^


**LIST NOT CLOSED**
This list is missing a closing bracket or has a syntax error.
Lists must be closed with **]** and list items must be separated by commas.
For example:     [1, 2, 3]

Here is the problematic code:
**nominal_import_long_package.md:3:51:3:51:**
```roc
import design.Styles.Color exposing [Encoder as CE]
```
                                                  


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**nominal_import_long_package.md:7:1:7:3:**
```roc
~~~
```
^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**nominal_import_long_package.md:7:2:7:4:**
```roc
~~~
```
 ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**nominal_import_long_package.md:7:3:7:4:**
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

**UNDECLARED TYPE**
The type ``CE`` is not declared in this scope.

This type is referenced here:
**nominal_import_long_package.md:5:7:5:9:**
```roc
red : CE
```
      ^^


**NOT IMPLEMENTED**
This feature is not yet implemented or doesn't have a proper error report yet: ...
Let us know if you want to help!

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
KwImport(3:1-3:7),LowerIdent(3:8-3:14),NoSpaceDotUpperIdent(3:14-3:21),NoSpaceDotUpperIdent(3:21-3:27),KwExposing(3:28-3:36),OpenSquare(3:37-3:38),UpperIdent(3:38-3:45),KwAs(3:46-3:48),UpperIdent(3:49-3:51),CloseSquare(3:51-3:52),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(5:1-5:4),OpColon(5:5-5:6),UpperIdent(5:7-5:9),Newline(1:1-1:1),
LowerIdent(6:1-6:4),OpAssign(6:5-6:6),TripleDot(6:7-6:10),Newline(6:12-6:28),
MalformedUnknownToken(7:1-7:2),MalformedUnknownToken(7:2-7:3),MalformedUnknownToken(7:3-7:4),EndOfFile(7:4-7:4),
~~~
# PARSE
~~~clojure
(file @1.1-7.4
	(module @1.1-1.13
		(exposes @1.8-1.13
			(exposed-lower-ident (text "red"))))
	(statements
		(s-import @3.1-3.21 (raw "design.Styles"))
		(e-malformed @3.21-3.36 (reason "expr_unexpected_token"))
		(e-malformed @3.28-3.38 (reason "expr_unexpected_token"))
		(e-malformed @1.1-1.1 (reason "expected_expr_close_square_or_comma"))
		(s-type-anno @1.1-1.1 (name "red")
			(ty @5.7-5.9 (name "CE")))
		(s-decl @6.1-6.10
			(p-ident @6.1-6.4 (raw "red"))
			(e-ellipsis))
		(e-malformed @7.1-7.3 (reason "expr_unexpected_token"))
		(e-malformed @7.2-7.4 (reason "expr_unexpected_token"))
		(e-malformed @7.3-7.4 (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc
module [red]

import design.Styles

red : CE
red = ... # not implemented

~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @6.1-6.4 (ident "red"))
		(e-runtime-error (tag "not_implemented"))
		(annotation @6.1-6.4
			(declared-type
				(ty @5.7-5.9 (name "CE")))))
	(s-import @3.1-3.21 (module "design.Styles") (qualifier "design")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.4 (type "Error")))
	(expressions
		(expr @6.7-6.10 (type "Error"))))
~~~

# META
~~~ini
description=Example of a simple nominal tag union
type=file
~~~
# SOURCE
~~~roc
module [Color, blue]

Color := [Red, Green, Blue]

blue : Color
blue = Color.Blue
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - nominal_tag_simple.md:7:1:7:3
UNEXPECTED TOKEN IN EXPRESSION - nominal_tag_simple.md:7:2:7:4
UNEXPECTED TOKEN IN EXPRESSION - nominal_tag_simple.md:7:3:7:4
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**nominal_tag_simple.md:7:1:7:3:**
```roc
~~~
```
^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**nominal_tag_simple.md:7:2:7:4:**
```roc
~~~
```
 ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**nominal_tag_simple.md:7:3:7:4:**
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
KwModule(1:1-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:14),Comma(1:14-1:15),LowerIdent(1:16-1:20),CloseSquare(1:20-1:21),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(3:1-3:6),OpColonEqual(3:7-3:9),OpenSquare(3:10-3:11),UpperIdent(3:11-3:14),Comma(3:14-3:15),UpperIdent(3:16-3:21),Comma(3:21-3:22),UpperIdent(3:23-3:27),CloseSquare(3:27-3:28),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(5:1-5:5),OpColon(5:6-5:7),UpperIdent(5:8-5:13),Newline(1:1-1:1),
LowerIdent(6:1-6:5),OpAssign(6:6-6:7),UpperIdent(6:8-6:13),NoSpaceDotUpperIdent(6:13-6:18),Newline(1:1-1:1),
MalformedUnknownToken(7:1-7:2),MalformedUnknownToken(7:2-7:3),MalformedUnknownToken(7:3-7:4),EndOfFile(7:4-7:4),
~~~
# PARSE
~~~clojure
(file @1.1-7.4
	(module @1.1-1.21
		(exposes @1.8-1.21
			(exposed-upper-ident (text "Color"))
			(exposed-lower-ident (text "blue"))))
	(statements
		(s-type-decl @3.1-3.28
			(header @3.1-3.6 (name "Color")
				(args))
			(ty-tag-union @3.10-3.28
				(tags
					(ty @3.11-3.14 (name "Red"))
					(ty @3.16-3.21 (name "Green"))
					(ty @3.23-3.27 (name "Blue")))))
		(s-type-anno @1.1-1.1 (name "blue")
			(ty @5.8-5.13 (name "Color")))
		(s-decl @6.1-6.18
			(p-ident @6.1-6.5 (raw "blue"))
			(e-tag @6.8-6.18 (raw "Color.Blue")))
		(e-malformed @7.1-7.3 (reason "expr_unexpected_token"))
		(e-malformed @7.2-7.4 (reason "expr_unexpected_token"))
		(e-malformed @7.3-7.4 (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc
module [Color, blue]

Color : [Red, Green, Blue]

blue : Color
blue = Blue

~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @6.1-6.5 (ident "blue"))
		(e-tag @6.8-6.18 (name "Blue"))
		(annotation @6.1-6.5
			(declared-type
				(ty @5.8-5.13 (name "Color")))))
	(s-nominal-decl @3.1-3.28 (match "TODO")
		(ty-header @3.1-3.6 (name "Color"))
		(ty-tag-union @3.10-3.28
			(ty @3.11-3.14 (name "Red"))
			(ty @3.16-3.21 (name "Green"))
			(ty @3.23-3.27 (name "Blue")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.5 (type "[Blue]*")))
	(expressions
		(expr @6.8-6.18 (type "[Blue]*"))))
~~~

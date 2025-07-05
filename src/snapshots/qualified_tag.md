# META
~~~ini
description=Simple qualified tag test
type=file
~~~
# SOURCE
~~~roc
module [Color]

Color := [Red, Blue]

test = Color.Red
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - qualified_tag.md:6:1:6:3
UNEXPECTED TOKEN IN EXPRESSION - qualified_tag.md:6:2:6:4
UNEXPECTED TOKEN IN EXPRESSION - qualified_tag.md:6:3:6:4
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**qualified_tag.md:6:1:6:3:**
```roc
~~~
```
^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**qualified_tag.md:6:2:6:4:**
```roc
~~~
```
 ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**qualified_tag.md:6:3:6:4:**
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
KwModule(1:1-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:14),CloseSquare(1:14-1:15),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(3:1-3:6),OpColonEqual(3:7-3:9),OpenSquare(3:10-3:11),UpperIdent(3:11-3:14),Comma(3:14-3:15),UpperIdent(3:16-3:20),CloseSquare(3:20-3:21),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(5:1-5:5),OpAssign(5:6-5:7),UpperIdent(5:8-5:13),NoSpaceDotUpperIdent(5:13-5:17),Newline(1:1-1:1),
MalformedUnknownToken(6:1-6:2),MalformedUnknownToken(6:2-6:3),MalformedUnknownToken(6:3-6:4),EndOfFile(6:4-6:4),
~~~
# PARSE
~~~clojure
(file @1.1-6.4
	(module @1.1-1.15
		(exposes @1.8-1.15
			(exposed-upper-ident (text "Color"))))
	(statements
		(s-type-decl @3.1-3.21
			(header @3.1-3.6 (name "Color")
				(args))
			(ty-tag-union @3.10-3.21
				(tags
					(ty @3.11-3.14 (name "Red"))
					(ty @3.16-3.20 (name "Blue")))))
		(s-decl @5.1-5.17
			(p-ident @5.1-5.5 (raw "test"))
			(e-tag @5.8-5.17 (raw "Color.Red")))
		(e-malformed @6.1-6.3 (reason "expr_unexpected_token"))
		(e-malformed @6.2-6.4 (reason "expr_unexpected_token"))
		(e-malformed @6.3-6.4 (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc
module [Color]

Color : [Red, Blue]

test = Red

~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @5.1-5.5 (ident "test"))
		(e-tag @5.8-5.17 (name "Red")))
	(s-nominal-decl @3.1-3.21 (match "TODO")
		(ty-header @3.1-3.6 (name "Color"))
		(ty-tag-union @3.10-3.21
			(ty @3.11-3.14 (name "Red"))
			(ty @3.16-3.20 (name "Blue")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @5.1-5.5 (type "[Red]*")))
	(expressions
		(expr @5.8-5.17 (type "[Red]*"))))
~~~

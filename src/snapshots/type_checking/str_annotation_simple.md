# META
~~~ini
description=Simple Str type annotation
type=file
~~~
# SOURCE
~~~roc
module []

x : Str
x = "hello"
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - str_annotation_simple.md:5:1:5:3
UNEXPECTED TOKEN IN EXPRESSION - str_annotation_simple.md:5:2:5:4
UNEXPECTED TOKEN IN EXPRESSION - str_annotation_simple.md:5:3:5:4
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**str_annotation_simple.md:5:1:5:3:**
```roc
~~~
```
^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**str_annotation_simple.md:5:2:5:4:**
```roc
~~~
```
 ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**str_annotation_simple.md:5:3:5:4:**
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
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:2),OpColon(3:3-3:4),UpperIdent(3:5-3:8),Newline(1:1-1:1),
LowerIdent(4:1-4:2),OpAssign(4:3-4:4),StringStart(4:5-4:6),StringPart(4:6-4:11),StringEnd(4:11-4:12),Newline(1:1-1:1),
MalformedUnknownToken(5:1-5:2),MalformedUnknownToken(5:2-5:3),MalformedUnknownToken(5:3-5:4),EndOfFile(5:4-5:4),
~~~
# PARSE
~~~clojure
(file @1.1-5.4
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-type-anno @1.1-1.1 (name "x")
			(ty @3.5-3.8 (name "Str")))
		(s-decl @4.1-4.12
			(p-ident @4.1-4.2 (raw "x"))
			(e-string @4.5-4.12
				(e-string-part @4.6-4.11 (raw "hello"))))
		(e-malformed @5.1-5.3 (reason "expr_unexpected_token"))
		(e-malformed @5.2-5.4 (reason "expr_unexpected_token"))
		(e-malformed @5.3-5.4 (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc
module []

x : Str
x = "hello"

~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.2 (ident "x"))
		(e-string @4.5-4.12
			(e-literal @4.6-4.11 (string "hello")))
		(annotation @4.1-4.2
			(declared-type
				(ty @3.5-3.8 (name "Str"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.2 (type "Str")))
	(expressions
		(expr @4.5-4.12 (type "Str"))))
~~~

# META
~~~ini
description=Dec type annotation
type=file
~~~
# SOURCE
~~~roc
module []

x : Dec
x = 123.456
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - dec_annotation.md:5:1:5:3
UNEXPECTED TOKEN IN EXPRESSION - dec_annotation.md:5:2:5:4
UNEXPECTED TOKEN IN EXPRESSION - dec_annotation.md:5:3:5:4
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**dec_annotation.md:5:1:5:3:**
```roc
~~~
```
^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**dec_annotation.md:5:2:5:4:**
```roc
~~~
```
 ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**dec_annotation.md:5:3:5:4:**
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
LowerIdent(4:1-4:2),OpAssign(4:3-4:4),Float(4:5-4:12),Newline(1:1-1:1),
MalformedUnknownToken(5:1-5:2),MalformedUnknownToken(5:2-5:3),MalformedUnknownToken(5:3-5:4),EndOfFile(5:4-5:4),
~~~
# PARSE
~~~clojure
(file @1.1-5.4
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-type-anno @1.1-1.1 (name "x")
			(ty @3.5-3.8 (name "Dec")))
		(s-decl @4.1-4.12
			(p-ident @4.1-4.2 (raw "x"))
			(e-frac @4.5-4.12 (raw "123.456")))
		(e-malformed @5.1-5.3 (reason "expr_unexpected_token"))
		(e-malformed @5.2-5.4 (reason "expr_unexpected_token"))
		(e-malformed @5.3-5.4 (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc
module []

x : Dec
x = 123.456

~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.2 (ident "x"))
		(e-frac-dec @4.5-4.12 (value "123.456"))
		(annotation @4.1-4.2
			(declared-type
				(ty @3.5-3.8 (name "Dec"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.2 (type "Dec")))
	(expressions
		(expr @4.5-4.12 (type "Dec"))))
~~~

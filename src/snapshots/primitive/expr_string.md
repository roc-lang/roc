# META
~~~ini
description=A primitive
type=file
~~~
# SOURCE
~~~roc
module [foo]
name = "luc"
foo = "hello ${name}"
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - expr_string.md:4:1:4:3
UNEXPECTED TOKEN IN EXPRESSION - expr_string.md:4:2:4:4
UNEXPECTED TOKEN IN EXPRESSION - expr_string.md:4:3:4:4
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**expr_string.md:4:1:4:3:**
```roc
~~~
```
^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**expr_string.md:4:2:4:4:**
```roc
~~~
```
 ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**expr_string.md:4:3:4:4:**
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
LowerIdent(2:1-2:5),OpAssign(2:6-2:7),StringStart(2:8-2:9),StringPart(2:9-2:12),StringEnd(2:12-2:13),Newline(1:1-1:1),
LowerIdent(3:1-3:4),OpAssign(3:5-3:6),StringStart(3:7-3:8),StringPart(3:8-3:14),OpenStringInterpolation(3:14-3:16),LowerIdent(3:16-3:20),CloseStringInterpolation(3:20-3:21),StringPart(3:21-3:21),StringEnd(3:21-3:22),Newline(1:1-1:1),
MalformedUnknownToken(4:1-4:2),MalformedUnknownToken(4:2-4:3),MalformedUnknownToken(4:3-4:4),EndOfFile(4:4-4:4),
~~~
# PARSE
~~~clojure
(file @1.1-4.4
	(module @1.1-1.13
		(exposes @1.8-1.13
			(exposed-lower-ident (text "foo"))))
	(statements
		(s-decl @2.1-2.13
			(p-ident @2.1-2.5 (raw "name"))
			(e-string @2.8-2.13
				(e-string-part @2.9-2.12 (raw "luc"))))
		(s-decl @3.1-3.22
			(p-ident @3.1-3.4 (raw "foo"))
			(e-string @3.7-3.22
				(e-string-part @3.8-3.14 (raw "hello "))
				(e-ident @3.16-3.20 (raw "name"))
				(e-string-part @3.21-3.21 (raw ""))))
		(e-malformed @4.1-4.3 (reason "expr_unexpected_token"))
		(e-malformed @4.2-4.4 (reason "expr_unexpected_token"))
		(e-malformed @4.3-4.4 (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc
module [foo]
name = "luc"
foo = "hello ${name}"

~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @2.1-2.5 (ident "name"))
		(e-string @2.8-2.13
			(e-literal @2.9-2.12 (string "luc"))))
	(d-let
		(p-assign @3.1-3.4 (ident "foo"))
		(e-string @3.7-3.22
			(e-literal @3.8-3.14 (string "hello "))
			(e-lookup-local @3.16-3.20
				(pattern @2.1-2.5))
			(e-literal @3.21-3.21 (string "")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @2.1-2.5 (type "Str"))
		(patt @3.1-3.4 (type "Str")))
	(expressions
		(expr @2.8-2.13 (type "Str"))
		(expr @3.7-3.22 (type "Str"))))
~~~

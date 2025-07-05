# META
~~~ini
description=A simple module with no blanks
type=file
~~~
# SOURCE
~~~roc
module [hello!, world]
import pf.Stdout
hello! = Stdout.line!("Hello")
world = "World"
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - simple_module_no_blanks.md:5:1:5:3
UNEXPECTED TOKEN IN EXPRESSION - simple_module_no_blanks.md:5:2:5:4
UNEXPECTED TOKEN IN EXPRESSION - simple_module_no_blanks.md:5:3:5:4
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**simple_module_no_blanks.md:5:1:5:3:**
```roc
~~~
```
^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**simple_module_no_blanks.md:5:2:5:4:**
```roc
~~~
```
 ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**simple_module_no_blanks.md:5:3:5:4:**
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
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:15),Comma(1:15-1:16),LowerIdent(1:17-1:22),CloseSquare(1:22-1:23),Newline(1:1-1:1),
KwImport(2:1-2:7),LowerIdent(2:8-2:10),NoSpaceDotUpperIdent(2:10-2:17),Newline(1:1-1:1),
LowerIdent(3:1-3:7),OpAssign(3:8-3:9),UpperIdent(3:10-3:16),NoSpaceDotLowerIdent(3:16-3:22),NoSpaceOpenRound(3:22-3:23),StringStart(3:23-3:24),StringPart(3:24-3:29),StringEnd(3:29-3:30),CloseRound(3:30-3:31),Newline(1:1-1:1),
LowerIdent(4:1-4:6),OpAssign(4:7-4:8),StringStart(4:9-4:10),StringPart(4:10-4:15),StringEnd(4:15-4:16),Newline(1:1-1:1),
MalformedUnknownToken(5:1-5:2),MalformedUnknownToken(5:2-5:3),MalformedUnknownToken(5:3-5:4),EndOfFile(5:4-5:4),
~~~
# PARSE
~~~clojure
(file @1.1-5.4
	(module @1.1-1.23
		(exposes @1.8-1.23
			(exposed-lower-ident (text "hello!"))
			(exposed-lower-ident (text "world"))))
	(statements
		(s-import @2.1-2.17 (raw "pf.Stdout"))
		(s-decl @3.1-3.31
			(p-ident @3.1-3.7 (raw "hello!"))
			(e-apply @3.10-3.31
				(e-ident @3.10-3.22 (raw "Stdout.line!"))
				(e-string @3.23-3.30
					(e-string-part @3.24-3.29 (raw "Hello")))))
		(s-decl @4.1-4.16
			(p-ident @4.1-4.6 (raw "world"))
			(e-string @4.9-4.16
				(e-string-part @4.10-4.15 (raw "World"))))
		(e-malformed @5.1-5.3 (reason "expr_unexpected_token"))
		(e-malformed @5.2-5.4 (reason "expr_unexpected_token"))
		(e-malformed @5.3-5.4 (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc
module [hello!, world]
import pf.Stdout
hello! = Stdout.line!("Hello")
world = "World"

~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @3.1-3.7 (ident "hello!"))
		(e-call @3.10-3.31
			(e-lookup-external
				(ext-decl @3.10-3.22 (ident "pf.Stdout.line!") (kind "value")))
			(e-string @3.23-3.30
				(e-literal @3.24-3.29 (string "Hello")))))
	(d-let
		(p-assign @4.1-4.6 (ident "world"))
		(e-string @4.9-4.16
			(e-literal @4.10-4.15 (string "World"))))
	(s-import @2.1-2.17 (module "pf.Stdout") (qualifier "pf")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.7 (type "*"))
		(patt @4.1-4.6 (type "Str")))
	(expressions
		(expr @3.10-3.31 (type "*"))
		(expr @4.9-4.16 (type "Str"))))
~~~

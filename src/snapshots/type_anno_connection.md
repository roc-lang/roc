# META
~~~ini
description=Type annotation connection to definitions
type=file
~~~
# SOURCE
~~~roc
module [add_one, my_number]

add_one : U64 -> U64
add_one = |x| x + 1

my_number : U64
my_number = add_one(42)
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - type_anno_connection.md:8:1:8:3
UNEXPECTED TOKEN IN EXPRESSION - type_anno_connection.md:8:2:8:4
UNEXPECTED TOKEN IN EXPRESSION - type_anno_connection.md:8:3:8:4
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**type_anno_connection.md:8:1:8:3:**
```roc
~~~
```
^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**type_anno_connection.md:8:2:8:4:**
```roc
~~~
```
 ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**type_anno_connection.md:8:3:8:4:**
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
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:16),Comma(1:16-1:17),LowerIdent(1:18-1:27),CloseSquare(1:27-1:28),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:8),OpColon(3:9-3:10),UpperIdent(3:11-3:14),OpArrow(3:15-3:17),UpperIdent(3:18-3:21),Newline(1:1-1:1),
LowerIdent(4:1-4:8),OpAssign(4:9-4:10),OpBar(4:11-4:12),LowerIdent(4:12-4:13),OpBar(4:13-4:14),LowerIdent(4:15-4:16),OpPlus(4:17-4:18),Int(4:19-4:20),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(6:1-6:10),OpColon(6:11-6:12),UpperIdent(6:13-6:16),Newline(1:1-1:1),
LowerIdent(7:1-7:10),OpAssign(7:11-7:12),LowerIdent(7:13-7:20),NoSpaceOpenRound(7:20-7:21),Int(7:21-7:23),CloseRound(7:23-7:24),Newline(1:1-1:1),
MalformedUnknownToken(8:1-8:2),MalformedUnknownToken(8:2-8:3),MalformedUnknownToken(8:3-8:4),EndOfFile(8:4-8:4),
~~~
# PARSE
~~~clojure
(file @1.1-8.4
	(module @1.1-1.28
		(exposes @1.8-1.28
			(exposed-lower-ident (text "add_one"))
			(exposed-lower-ident (text "my_number"))))
	(statements
		(s-type-anno @1.1-1.1 (name "add_one")
			(ty-fn @3.11-3.21
				(ty @3.11-3.14 (name "U64"))
				(ty @3.18-3.21 (name "U64"))))
		(s-decl @4.1-6.10
			(p-ident @4.1-4.8 (raw "add_one"))
			(e-lambda @4.11-6.10
				(args
					(p-ident @4.12-4.13 (raw "x")))
				(e-binop @4.15-6.10 (op "+")
					(e-ident @4.15-4.16 (raw "x"))
					(e-int @4.19-4.20 (raw "1")))))
		(s-type-anno @1.1-1.1 (name "my_number")
			(ty @6.13-6.16 (name "U64")))
		(s-decl @7.1-7.24
			(p-ident @7.1-7.10 (raw "my_number"))
			(e-apply @7.13-7.24
				(e-ident @7.13-7.20 (raw "add_one"))
				(e-int @7.21-7.23 (raw "42"))))
		(e-malformed @8.1-8.3 (reason "expr_unexpected_token"))
		(e-malformed @8.2-8.4 (reason "expr_unexpected_token"))
		(e-malformed @8.3-8.4 (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc
module [add_one, my_number]

add_one : U64 -> U64
add_one = |x| x + 1

my_number : U64
my_number = add_one(42)

~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.8 (ident "add_one"))
		(e-lambda @4.11-6.10
			(args
				(p-assign @4.12-4.13 (ident "x")))
			(e-binop @4.15-6.10 (op "add")
				(e-lookup-local @4.15-4.16
					(pattern @4.12-4.13))
				(e-int @4.19-4.20 (value "1"))))
		(annotation @4.1-4.8
			(declared-type
				(ty-fn @3.11-3.21 (effectful false)
					(ty @3.11-3.14 (name "U64"))
					(ty @3.18-3.21 (name "U64"))))))
	(d-let
		(p-assign @7.1-7.10 (ident "my_number"))
		(e-call @7.13-7.24
			(e-lookup-local @7.13-7.20
				(pattern @4.1-4.8))
			(e-int @7.21-7.23 (value "42")))
		(annotation @7.1-7.10
			(declared-type
				(ty @6.13-6.16 (name "U64"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.8 (type "U64 -> U64"))
		(patt @7.1-7.10 (type "U64")))
	(expressions
		(expr @4.11-6.10 (type "U64 -> U64"))
		(expr @7.13-7.24 (type "U64"))))
~~~

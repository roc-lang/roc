# META
~~~ini
description=A primitive
type=file
~~~
# SOURCE
~~~roc
module [Foo]

Foo(a,b) : (a,b,Str,U64)
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - stmt_type_decl.md:4:1:4:3
UNEXPECTED TOKEN IN EXPRESSION - stmt_type_decl.md:4:2:4:4
UNEXPECTED TOKEN IN EXPRESSION - stmt_type_decl.md:4:3:4:4
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**stmt_type_decl.md:4:1:4:3:**
```roc
~~~
```
^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**stmt_type_decl.md:4:2:4:4:**
```roc
~~~
```
 ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**stmt_type_decl.md:4:3:4:4:**
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
KwModule(1:1-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:12),CloseSquare(1:12-1:13),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(3:1-3:4),NoSpaceOpenRound(3:4-3:5),LowerIdent(3:5-3:6),Comma(3:6-3:7),LowerIdent(3:7-3:8),CloseRound(3:8-3:9),OpColon(3:10-3:11),OpenRound(3:12-3:13),LowerIdent(3:13-3:14),Comma(3:14-3:15),LowerIdent(3:15-3:16),Comma(3:16-3:17),UpperIdent(3:17-3:20),Comma(3:20-3:21),UpperIdent(3:21-3:24),CloseRound(3:24-3:25),Newline(1:1-1:1),
MalformedUnknownToken(4:1-4:2),MalformedUnknownToken(4:2-4:3),MalformedUnknownToken(4:3-4:4),EndOfFile(4:4-4:4),
~~~
# PARSE
~~~clojure
(file @1.1-4.4
	(module @1.1-1.13
		(exposes @1.8-1.13
			(exposed-upper-ident (text "Foo"))))
	(statements
		(s-type-decl @3.1-3.25
			(header @3.1-3.9 (name "Foo")
				(args
					(ty-var @3.5-3.6 (raw "a"))
					(ty-var @3.7-3.8 (raw "b"))))
			(ty-tuple @3.12-3.25
				(ty-var @3.13-3.14 (raw "a"))
				(ty-var @3.15-3.16 (raw "b"))
				(ty @3.17-3.20 (name "Str"))
				(ty @3.21-3.24 (name "U64"))))
		(e-malformed @4.1-4.3 (reason "expr_unexpected_token"))
		(e-malformed @4.2-4.4 (reason "expr_unexpected_token"))
		(e-malformed @4.3-4.4 (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc
module [Foo]

Foo(a, b) : (a, b, Str, U64)

~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl @3.1-3.25 (where "TODO")
		(ty-header @3.1-3.9 (name "Foo")
			(ty-args
				(ty-var @3.5-3.6 (name "a"))
				(ty-var @3.7-3.8 (name "b"))))
		(ty-tuple @3.12-3.25
			(ty-var @3.13-3.14 (name "a"))
			(ty-var @3.15-3.16 (name "b"))
			(ty @3.17-3.20 (name "Str"))
			(ty @3.21-3.24 (name "U64")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~

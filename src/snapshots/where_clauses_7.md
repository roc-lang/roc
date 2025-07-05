# META
~~~ini
description=where_clauses (7)
type=file
~~~
# SOURCE
~~~roc
module [Hash]

Hash(a) # After header
	: # After colon
		a # After var
			where # After where
				a.hash(hasher) # After method
					-> # After arrow
						hasher, # After first clause
				hasher.Hasher,

Decode(a) : a
	where
		module(a).decode( # After method args open
			List(U8), # After method arg
		) -> a,
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - where_clauses_7.md:17:1:17:3
UNEXPECTED TOKEN IN EXPRESSION - where_clauses_7.md:17:2:17:4
UNEXPECTED TOKEN IN EXPRESSION - where_clauses_7.md:17:3:17:4
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**where_clauses_7.md:17:1:17:3:**
```roc
~~~
```
^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**where_clauses_7.md:17:2:17:4:**
```roc
~~~
```
 ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**where_clauses_7.md:17:3:17:4:**
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
KwModule(1:1-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:13),CloseSquare(1:13-1:14),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(3:1-3:5),NoSpaceOpenRound(3:5-3:6),LowerIdent(3:6-3:7),CloseRound(3:7-3:8),Newline(3:10-3:23),
OpColon(4:2-4:3),Newline(4:5-4:17),
LowerIdent(5:3-5:4),Newline(5:6-5:16),
KwWhere(6:4-6:9),Newline(6:11-6:23),
LowerIdent(7:5-7:6),NoSpaceDotLowerIdent(7:6-7:11),NoSpaceOpenRound(7:11-7:12),LowerIdent(7:12-7:18),CloseRound(7:18-7:19),Newline(7:21-7:34),
OpArrow(8:6-8:8),Newline(8:10-8:22),
LowerIdent(9:7-9:13),Comma(9:13-9:14),Newline(9:16-9:35),
LowerIdent(10:5-10:11),NoSpaceDotUpperIdent(10:11-10:18),Comma(10:18-10:19),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(12:1-12:7),NoSpaceOpenRound(12:7-12:8),LowerIdent(12:8-12:9),CloseRound(12:9-12:10),OpColon(12:11-12:12),LowerIdent(12:13-12:14),Newline(1:1-1:1),
KwWhere(13:2-13:7),Newline(1:1-1:1),
KwModule(14:3-14:9),NoSpaceOpenRound(14:9-14:10),LowerIdent(14:10-14:11),CloseRound(14:11-14:12),NoSpaceDotLowerIdent(14:12-14:19),NoSpaceOpenRound(14:19-14:20),Newline(14:22-14:45),
UpperIdent(15:4-15:8),NoSpaceOpenRound(15:8-15:9),UpperIdent(15:9-15:11),CloseRound(15:11-15:12),Comma(15:12-15:13),Newline(15:15-15:32),
CloseRound(16:3-16:4),OpArrow(16:5-16:7),LowerIdent(16:8-16:9),Comma(16:9-16:10),Newline(1:1-1:1),
MalformedUnknownToken(17:1-17:2),MalformedUnknownToken(17:2-17:3),MalformedUnknownToken(17:3-17:4),EndOfFile(17:4-17:4),
~~~
# PARSE
~~~clojure
(file @1.1-17.4
	(module @1.1-1.14
		(exposes @1.8-1.14
			(exposed-upper-ident (text "Hash"))))
	(statements
		(s-type-decl @3.1-12.7
			(header @3.1-3.8 (name "Hash")
				(args
					(ty-var @3.6-3.7 (raw "a"))))
			(ty-var @5.3-5.4 (raw "a")))
		(s-type-decl @12.1-17.2
			(header @12.1-12.10 (name "Decode")
				(args
					(ty-var @12.8-12.9 (raw "a"))))
			(ty-var @12.13-12.14 (raw "a")))
		(e-malformed @17.1-17.3 (reason "expr_unexpected_token"))
		(e-malformed @17.2-17.4 (reason "expr_unexpected_token"))
		(e-malformed @17.3-17.4 (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc
module [Hash]

Hash(a) # After header
	: # After colon
		a # After var
			where # After where
				a.hash(hasher) # After method
					-> # After arrow
						hasher, # After first clause
				hasher.Hasher,

Decode(a) : a
	where
		module(a).decode( # After method args open
			List(U8), # After method arg
		) -> a,

~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl @3.1-12.7 (where "TODO")
		(ty-header @3.1-3.8 (name "Hash")
			(ty-args
				(ty-var @3.6-3.7 (name "a"))))
		(ty-var @5.3-5.4 (name "a")))
	(s-alias-decl @12.1-17.2 (where "TODO")
		(ty-header @12.1-12.10 (name "Decode")
			(ty-args
				(ty-var @12.8-12.9 (name "a"))))
		(ty-var @12.13-12.14 (name "a"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~

# META
~~~ini
description=where_clauses (1)
type=file
~~~
# SOURCE
~~~roc
module [Hash]

Hash(a) : a
	where
		a.hash(hasher) -> hasher,
		hasher.Hasher,

Decode(a) : a
	where
		module(a).decode(List(U8)) -> a,
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - where_clauses_1.md:11:1:11:3
UNEXPECTED TOKEN IN EXPRESSION - where_clauses_1.md:11:2:11:4
UNEXPECTED TOKEN IN EXPRESSION - where_clauses_1.md:11:3:11:4
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**where_clauses_1.md:11:1:11:3:**
```roc
~~~
```
^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**where_clauses_1.md:11:2:11:4:**
```roc
~~~
```
 ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**where_clauses_1.md:11:3:11:4:**
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
UpperIdent(3:1-3:5),NoSpaceOpenRound(3:5-3:6),LowerIdent(3:6-3:7),CloseRound(3:7-3:8),OpColon(3:9-3:10),LowerIdent(3:11-3:12),Newline(1:1-1:1),
KwWhere(4:2-4:7),Newline(1:1-1:1),
LowerIdent(5:3-5:4),NoSpaceDotLowerIdent(5:4-5:9),NoSpaceOpenRound(5:9-5:10),LowerIdent(5:10-5:16),CloseRound(5:16-5:17),OpArrow(5:18-5:20),LowerIdent(5:21-5:27),Comma(5:27-5:28),Newline(1:1-1:1),
LowerIdent(6:3-6:9),NoSpaceDotUpperIdent(6:9-6:16),Comma(6:16-6:17),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(8:1-8:7),NoSpaceOpenRound(8:7-8:8),LowerIdent(8:8-8:9),CloseRound(8:9-8:10),OpColon(8:11-8:12),LowerIdent(8:13-8:14),Newline(1:1-1:1),
KwWhere(9:2-9:7),Newline(1:1-1:1),
KwModule(10:3-10:9),NoSpaceOpenRound(10:9-10:10),LowerIdent(10:10-10:11),CloseRound(10:11-10:12),NoSpaceDotLowerIdent(10:12-10:19),NoSpaceOpenRound(10:19-10:20),UpperIdent(10:20-10:24),NoSpaceOpenRound(10:24-10:25),UpperIdent(10:25-10:27),CloseRound(10:27-10:28),CloseRound(10:28-10:29),OpArrow(10:30-10:32),LowerIdent(10:33-10:34),Comma(10:34-10:35),Newline(1:1-1:1),
MalformedUnknownToken(11:1-11:2),MalformedUnknownToken(11:2-11:3),MalformedUnknownToken(11:3-11:4),EndOfFile(11:4-11:4),
~~~
# PARSE
~~~clojure
(file @1.1-11.4
	(module @1.1-1.14
		(exposes @1.8-1.14
			(exposed-upper-ident (text "Hash"))))
	(statements
		(s-type-decl @3.1-8.7
			(header @3.1-3.8 (name "Hash")
				(args
					(ty-var @3.6-3.7 (raw "a"))))
			(ty-var @3.11-3.12 (raw "a")))
		(s-type-decl @8.1-11.2
			(header @8.1-8.10 (name "Decode")
				(args
					(ty-var @8.8-8.9 (raw "a"))))
			(ty-var @8.13-8.14 (raw "a")))
		(e-malformed @11.1-11.3 (reason "expr_unexpected_token"))
		(e-malformed @11.2-11.4 (reason "expr_unexpected_token"))
		(e-malformed @11.3-11.4 (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc
module [Hash]

Hash(a) : a
	where
		a.hash(hasher) -> hasher,
		hasher.Hasher,

Decode(a) : a
	where
		module(a).decode(List(U8)) -> a,

~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl @3.1-8.7 (where "TODO")
		(ty-header @3.1-3.8 (name "Hash")
			(ty-args
				(ty-var @3.6-3.7 (name "a"))))
		(ty-var @3.11-3.12 (name "a")))
	(s-alias-decl @8.1-11.2 (where "TODO")
		(ty-header @8.1-8.10 (name "Decode")
			(ty-args
				(ty-var @8.8-8.9 (name "a"))))
		(ty-var @8.13-8.14 (name "a"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~

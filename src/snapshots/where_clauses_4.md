# META
~~~ini
description=where_clauses (4)
type=file
~~~
# SOURCE
~~~roc
module [decode]

import Decode exposing [Decode]

decodeThings : List(List(U8)) -> List(a)
	where a.Decode
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - where_clauses_4.md:7:1:7:3
UNEXPECTED TOKEN IN EXPRESSION - where_clauses_4.md:7:2:7:4
UNEXPECTED TOKEN IN EXPRESSION - where_clauses_4.md:7:3:7:4
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**where_clauses_4.md:7:1:7:3:**
```roc
~~~
```
^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**where_clauses_4.md:7:2:7:4:**
```roc
~~~
```
 ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**where_clauses_4.md:7:3:7:4:**
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
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:15),CloseSquare(1:15-1:16),Newline(1:1-1:1),
Newline(1:1-1:1),
KwImport(3:1-3:7),UpperIdent(3:8-3:14),KwExposing(3:15-3:23),OpenSquare(3:24-3:25),UpperIdent(3:25-3:31),CloseSquare(3:31-3:32),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(5:1-5:13),OpColon(5:14-5:15),UpperIdent(5:16-5:20),NoSpaceOpenRound(5:20-5:21),UpperIdent(5:21-5:25),NoSpaceOpenRound(5:25-5:26),UpperIdent(5:26-5:28),CloseRound(5:28-5:29),CloseRound(5:29-5:30),OpArrow(5:31-5:33),UpperIdent(5:34-5:38),NoSpaceOpenRound(5:38-5:39),LowerIdent(5:39-5:40),CloseRound(5:40-5:41),Newline(1:1-1:1),
KwWhere(6:2-6:7),LowerIdent(6:8-6:9),NoSpaceDotUpperIdent(6:9-6:16),Newline(1:1-1:1),
MalformedUnknownToken(7:1-7:2),MalformedUnknownToken(7:2-7:3),MalformedUnknownToken(7:3-7:4),EndOfFile(7:4-7:4),
~~~
# PARSE
~~~clojure
(file @1.1-7.4
	(module @1.1-1.16
		(exposes @1.8-1.16
			(exposed-lower-ident (text "decode"))))
	(statements
		(s-import @3.1-3.32 (raw "Decode")
			(exposing
				(exposed-upper-ident (text "Decode"))))
		(s-type-anno @5.1-7.2 (name "decodeThings")
			(ty-fn @5.16-5.41
				(ty-apply @5.16-5.30
					(ty @5.16-5.20 (name "List"))
					(ty-apply @5.21-5.29
						(ty @5.21-5.25 (name "List"))
						(ty @5.26-5.28 (name "U8"))))
				(ty-apply @5.34-5.41
					(ty @5.34-5.38 (name "List"))
					(ty-var @5.39-5.40 (raw "a")))))
		(e-malformed @7.1-7.3 (reason "expr_unexpected_token"))
		(e-malformed @7.2-7.4 (reason "expr_unexpected_token"))
		(e-malformed @7.3-7.4 (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc
module [decode]

import Decode exposing [Decode]

decodeThings : List(List(U8)) -> List(a)
	where
		a.Decode,

~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-import @3.1-3.32 (module "Decode")
		(exposes
			(exposed (name "Decode") (wildcard false)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~

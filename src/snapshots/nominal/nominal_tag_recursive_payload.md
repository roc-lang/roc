# META
~~~ini
description=Example of a recursive nominal tag union with payload
type=file
~~~
# SOURCE
~~~roc
module [ConsList, empty]

ConsList(a) := [Nil, Node(ConsList(a))]

empty : ConsList(a)
empty = ConsList.Nil
~~~
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - nominal_tag_recursive_payload.md:7:1:7:3
UNEXPECTED TOKEN IN EXPRESSION - nominal_tag_recursive_payload.md:7:2:7:4
UNEXPECTED TOKEN IN EXPRESSION - nominal_tag_recursive_payload.md:7:3:7:4
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**nominal_tag_recursive_payload.md:7:1:7:3:**
```roc
~~~
```
^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**nominal_tag_recursive_payload.md:7:2:7:4:**
```roc
~~~
```
 ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**nominal_tag_recursive_payload.md:7:3:7:4:**
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
KwModule(1:1-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:17),Comma(1:17-1:18),LowerIdent(1:19-1:24),CloseSquare(1:24-1:25),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(3:1-3:9),NoSpaceOpenRound(3:9-3:10),LowerIdent(3:10-3:11),CloseRound(3:11-3:12),OpColonEqual(3:13-3:15),OpenSquare(3:16-3:17),UpperIdent(3:17-3:20),Comma(3:20-3:21),UpperIdent(3:22-3:26),NoSpaceOpenRound(3:26-3:27),UpperIdent(3:27-3:35),NoSpaceOpenRound(3:35-3:36),LowerIdent(3:36-3:37),CloseRound(3:37-3:38),CloseRound(3:38-3:39),CloseSquare(3:39-3:40),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(5:1-5:6),OpColon(5:7-5:8),UpperIdent(5:9-5:17),NoSpaceOpenRound(5:17-5:18),LowerIdent(5:18-5:19),CloseRound(5:19-5:20),Newline(1:1-1:1),
LowerIdent(6:1-6:6),OpAssign(6:7-6:8),UpperIdent(6:9-6:17),NoSpaceDotUpperIdent(6:17-6:21),Newline(1:1-1:1),
MalformedUnknownToken(7:1-7:2),MalformedUnknownToken(7:2-7:3),MalformedUnknownToken(7:3-7:4),EndOfFile(7:4-7:4),
~~~
# PARSE
~~~clojure
(file @1.1-7.4
	(module @1.1-1.25
		(exposes @1.8-1.25
			(exposed-upper-ident (text "ConsList"))
			(exposed-lower-ident (text "empty"))))
	(statements
		(s-type-decl @3.1-3.40
			(header @3.1-3.12 (name "ConsList")
				(args
					(ty-var @3.10-3.11 (raw "a"))))
			(ty-tag-union @3.16-3.40
				(tags
					(ty @3.17-3.20 (name "Nil"))
					(ty-apply @3.22-3.39
						(ty @3.22-3.26 (name "Node"))
						(ty-apply @3.27-3.38
							(ty @3.27-3.35 (name "ConsList"))
							(ty-var @3.36-3.37 (raw "a")))))))
		(s-type-anno @5.1-6.6 (name "empty")
			(ty-apply @5.9-5.20
				(ty @5.9-5.17 (name "ConsList"))
				(ty-var @5.18-5.19 (raw "a"))))
		(s-decl @6.1-6.21
			(p-ident @6.1-6.6 (raw "empty"))
			(e-tag @6.9-6.21 (raw "ConsList.Nil")))
		(e-malformed @7.1-7.3 (reason "expr_unexpected_token"))
		(e-malformed @7.2-7.4 (reason "expr_unexpected_token"))
		(e-malformed @7.3-7.4 (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc
module [ConsList, empty]

ConsList(a) : [Nil, Node(ConsList(a))]

empty : ConsList(a)
empty = Nil

~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @6.1-6.6 (ident "empty"))
		(e-tag @6.9-6.21 (name "Nil"))
		(annotation @6.1-6.6
			(declared-type
				(ty-apply @5.9-5.20 (symbol "ConsList")
					(ty-var @5.18-5.19 (name "a"))))))
	(s-nominal-decl @3.1-3.40 (match "TODO")
		(ty-header @3.1-3.12 (name "ConsList")
			(ty-args
				(ty-var @3.10-3.11 (name "a"))))
		(ty-tag-union @3.16-3.40
			(ty @3.17-3.20 (name "Nil"))
			(ty-apply @3.22-3.39 (symbol "Node")
				(ty-apply @3.27-3.38 (symbol "ConsList")
					(ty-var @3.36-3.37 (name "a")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.6 (type "[Nil]*")))
	(expressions
		(expr @6.9-6.21 (type "[Nil]*"))))
~~~

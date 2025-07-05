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
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **.Nil** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**nominal_tag_recursive_payload.md:6:17:6:21:**
```roc
empty = ConsList.Nil
```
                ^^^^


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
LowerIdent(6:1-6:6),OpAssign(6:7-6:8),UpperIdent(6:9-6:17),NoSpaceDotUpperIdent(6:17-6:21),EndOfFile(6:21-6:21),
~~~
# PARSE
~~~clojure
(file @1.1-6.21
	(module @1.1-1.25
		(exposes @1.8-1.25
			(exposed-upper-ident (text "ConsList"))
			(exposed-lower-ident (text "empty"))))
	(statements
		(s-type-decl @3.1-5.6
			(header @3.1-3.12 (name "ConsList")
				(args
					(ty-var @3.10-3.11 (raw "a"))))
			(ty-tag-union @3.16-3.40
				(tags
					(ty (name "Nil"))
					(ty-apply @3.22-3.39
						(ty (name "Node"))
						(ty-apply @3.27-3.38
							(ty (name "ConsList"))
							(ty-var @3.36-3.37 (raw "a")))))))
		(s-type-anno @5.1-6.6 (name "empty")
			(ty-apply @5.9-5.20
				(ty (name "ConsList"))
				(ty-var @5.18-5.19 (raw "a"))))
		(s-decl @6.1-6.17
			(p-ident @6.1-6.6 (raw "empty"))
			(e-tag @6.9-6.17 (raw "ConsList")))
		(e-malformed @6.17-6.21 (reason "expr_unexpected_token"))))
~~~
# FORMATTED
~~~roc
module [ConsList, empty]

ConsList(a) : [Nil, Node(ConsList(a))]

empty : ConsList(a)
empty = ConsList
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @6.1-6.6 (ident "empty"))
		(e-tag @6.9-6.17 (name "ConsList") (args "TODO"))
		(annotation @6.1-6.6
			(declared-type
				(ty-apply @5.9-5.20 (symbol "ConsList")
					(ty-var @5.18-5.19 (name "a"))))))
	(s-type-decl @3.1-5.6
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
		(patt @6.1-6.6 (type "[ConsList]*")))
	(expressions
		(expr @6.9-6.17 (type "[ConsList]*"))))
~~~

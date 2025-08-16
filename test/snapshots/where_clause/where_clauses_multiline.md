# META
~~~ini
description=Where clause with multiline constraints
type=file
~~~
# SOURCE
~~~roc
module [process]

process : a, b -> c
	where
		module(a).convert : a -> c,
		module(b).transform : b -> c
process = ...
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **module** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

Here is the problematic code:
**where_clauses_multiline.md:6:3:6:9:**
```roc
		module(b).transform : b -> c
```
		^^^^^^


**PARSE ERROR**
A parsing error occurred: `expected_arrow`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**where_clauses_multiline.md:5:28:5:29:**
```roc
		module(a).convert : a -> c,
```
		                         ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**where_clauses_multiline.md:6:10:6:11:**
```roc
		module(b).transform : b -> c
```
		       ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**where_clauses_multiline.md:6:11:6:12:**
```roc
		module(b).transform : b -> c
```
		        ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**where_clauses_multiline.md:6:12:6:22:**
```roc
		module(b).transform : b -> c
```
		         ^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**where_clauses_multiline.md:6:23:6:24:**
```roc
		module(b).transform : b -> c
```
		                    ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**where_clauses_multiline.md:6:25:6:26:**
```roc
		module(b).transform : b -> c
```
		                      ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**where_clauses_multiline.md:6:27:6:29:**
```roc
		module(b).transform : b -> c
```
		                        ^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**where_clauses_multiline.md:6:30:6:31:**
```roc
		module(b).transform : b -> c
```
		                           ^


**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

**where_clauses_multiline.md:5:28:6:10:**
```roc
		module(a).convert : a -> c,
		module(b).transform : b -> c
```


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:16),CloseSquare(1:16-1:17),
LowerIdent(3:1-3:8),OpColon(3:9-3:10),LowerIdent(3:11-3:12),Comma(3:12-3:13),LowerIdent(3:14-3:15),OpArrow(3:16-3:18),LowerIdent(3:19-3:20),
KwWhere(4:2-4:7),
KwModule(5:3-5:9),NoSpaceOpenRound(5:9-5:10),LowerIdent(5:10-5:11),CloseRound(5:11-5:12),NoSpaceDotLowerIdent(5:12-5:20),OpColon(5:21-5:22),LowerIdent(5:23-5:24),OpArrow(5:25-5:27),LowerIdent(5:28-5:29),Comma(5:29-5:30),
KwModule(6:3-6:9),NoSpaceOpenRound(6:9-6:10),LowerIdent(6:10-6:11),CloseRound(6:11-6:12),NoSpaceDotLowerIdent(6:12-6:22),OpColon(6:23-6:24),LowerIdent(6:25-6:26),OpArrow(6:27-6:29),LowerIdent(6:30-6:31),
LowerIdent(7:1-7:8),OpAssign(7:9-7:10),TripleDot(7:11-7:14),EndOfFile(7:14-7:14),
~~~
# PARSE
~~~clojure
(file @1.1-7.14
	(module @1.1-1.17
		(exposes @1.8-1.17
			(exposed-lower-ident @1.9-1.16
				(text "process"))))
	(statements
		(s-type-anno @3.1-6.10 (name "process")
			(ty-fn @3.11-3.20
				(ty-var @3.11-3.12 (raw "a"))
				(ty-var @3.14-3.15 (raw "b"))
				(ty-var @3.19-3.20 (raw "c")))
			(where
				(method @5.3-6.10 (module-of "a") (name "convert")
					(args
						(ty-var @5.23-5.24 (raw "a")))
					(ty-malformed @5.28-6.10 (tag "expected_arrow")))))
		(s-malformed @6.10-6.11 (tag "statement_unexpected_token"))
		(s-malformed @6.11-6.12 (tag "statement_unexpected_token"))
		(s-malformed @6.12-6.22 (tag "statement_unexpected_token"))
		(s-malformed @6.23-6.24 (tag "statement_unexpected_token"))
		(s-malformed @6.25-6.26 (tag "statement_unexpected_token"))
		(s-malformed @6.27-6.29 (tag "statement_unexpected_token"))
		(s-malformed @6.30-6.31 (tag "statement_unexpected_token"))
		(s-decl @7.1-7.14
			(p-ident @7.1-7.8 (raw "process"))
			(e-ellipsis))))
~~~
# FORMATTED
~~~roc
module [process]

process : a, b -> c
	where
		module(a).convert : a -> 

process = ...
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @7.1-7.8 (ident "process"))
		(e-not-implemented @1.1-1.1))
	(s-type-anno @3.1-6.10 (name "process")
		(ty-fn @3.11-3.20 (effectful false)
			(ty-var @3.11-3.12 (name "a"))
			(ty-var @3.14-3.15 (name "b"))
			(ty-var @3.19-3.20 (name "c")))
		(where
			(method @5.3-6.10 (module-of "a") (ident "convert")
				(args
					(ty-var @5.23-5.24 (name "a")))
				(ty-malformed @5.28-6.10))))
	(ext-decl @5.3-6.10 (ident "module(a).convert") (kind "value")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @7.1-7.8 (type "_d")))
	(expressions
		(expr @1.1-1.1 (type "_d"))))
~~~

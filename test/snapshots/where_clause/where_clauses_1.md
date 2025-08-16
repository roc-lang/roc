# META
~~~ini
description=where_clauses (1)
type=file
~~~
# SOURCE
~~~roc
module [Hash, Decode]

Hash(a, hasher) : a
	where
		module(a).hash : hasher -> hasher,
		module(hasher).Hasher

Decode(a) : a where module(a).decode : List(U8) -> a
~~~
# EXPECTED
WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION - where_clauses_1.md:3:1:6:24
WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION - where_clauses_1.md:8:1:8:53
# PROBLEMS
**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **module** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

Here is the problematic code:
**where_clauses_1.md:6:3:6:9:**
```roc
		module(hasher).Hasher
```
		^^^^^^


**PARSE ERROR**
A parsing error occurred: `expected_arrow`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**where_clauses_1.md:5:30:5:36:**
```roc
		module(a).hash : hasher -> hasher,
```
		                           ^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**where_clauses_1.md:6:10:6:16:**
```roc
		module(hasher).Hasher
```
		       ^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**where_clauses_1.md:6:16:6:17:**
```roc
		module(hasher).Hasher
```
		             ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**where_clauses_1.md:6:17:6:24:**
```roc
		module(hasher).Hasher
```
		              ^^^^^^^


**WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION**
You cannot define a `where` clause inside a type declaration.

You're attempting do this here:
**where_clauses_1.md:3:1:6:10:**
```roc
Hash(a, hasher) : a
	where
		module(a).hash : hasher -> hasher,
		module(hasher).Hasher
```


**WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION**
You cannot define a `where` clause inside a type declaration.

You're attempting do this here:
**where_clauses_1.md:8:1:8:53:**
```roc
Decode(a) : a where module(a).decode : List(U8) -> a
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:13),Comma(1:13-1:14),UpperIdent(1:15-1:21),CloseSquare(1:21-1:22),
UpperIdent(3:1-3:5),NoSpaceOpenRound(3:5-3:6),LowerIdent(3:6-3:7),Comma(3:7-3:8),LowerIdent(3:9-3:15),CloseRound(3:15-3:16),OpColon(3:17-3:18),LowerIdent(3:19-3:20),
KwWhere(4:2-4:7),
KwModule(5:3-5:9),NoSpaceOpenRound(5:9-5:10),LowerIdent(5:10-5:11),CloseRound(5:11-5:12),NoSpaceDotLowerIdent(5:12-5:17),OpColon(5:18-5:19),LowerIdent(5:20-5:26),OpArrow(5:27-5:29),LowerIdent(5:30-5:36),Comma(5:36-5:37),
KwModule(6:3-6:9),NoSpaceOpenRound(6:9-6:10),LowerIdent(6:10-6:16),CloseRound(6:16-6:17),NoSpaceDotUpperIdent(6:17-6:24),
UpperIdent(8:1-8:7),NoSpaceOpenRound(8:7-8:8),LowerIdent(8:8-8:9),CloseRound(8:9-8:10),OpColon(8:11-8:12),LowerIdent(8:13-8:14),KwWhere(8:15-8:20),KwModule(8:21-8:27),NoSpaceOpenRound(8:27-8:28),LowerIdent(8:28-8:29),CloseRound(8:29-8:30),NoSpaceDotLowerIdent(8:30-8:37),OpColon(8:38-8:39),UpperIdent(8:40-8:44),NoSpaceOpenRound(8:44-8:45),UpperIdent(8:45-8:47),CloseRound(8:47-8:48),OpArrow(8:49-8:51),LowerIdent(8:52-8:53),EndOfFile(8:53-8:53),
~~~
# PARSE
~~~clojure
(file @1.1-8.53
	(module @1.1-1.22
		(exposes @1.8-1.22
			(exposed-upper-ident @1.9-1.13 (text "Hash"))
			(exposed-upper-ident @1.15-1.21 (text "Decode"))))
	(statements
		(s-type-decl @3.1-6.10
			(header @3.1-3.16 (name "Hash")
				(args
					(ty-var @3.6-3.7 (raw "a"))
					(ty-var @3.9-3.15 (raw "hasher"))))
			(ty-var @3.19-3.20 (raw "a"))
			(where
				(method @5.3-6.10 (module-of "a") (name "hash")
					(args
						(ty-var @5.20-5.26 (raw "hasher")))
					(ty-malformed @5.30-6.10 (tag "expected_arrow")))))
		(s-malformed @6.10-6.16 (tag "statement_unexpected_token"))
		(s-malformed @6.16-6.17 (tag "statement_unexpected_token"))
		(s-malformed @6.17-6.24 (tag "statement_unexpected_token"))
		(s-type-decl @8.1-8.53
			(header @8.1-8.10 (name "Decode")
				(args
					(ty-var @8.8-8.9 (raw "a"))))
			(ty-var @8.13-8.14 (raw "a"))
			(where
				(method @8.21-8.53 (module-of "a") (name "decode")
					(args
						(ty-apply @8.40-8.48
							(ty @8.40-8.44 (name "List"))
							(ty @8.45-8.47 (name "U8"))))
					(ty-var @8.52-8.53 (raw "a")))))))
~~~
# FORMATTED
~~~roc
module [Hash, Decode]

Hash(a, hasher) : a
	where
		module(a).hash : hasher -> 


Decode(a) : a where module(a).decode : List(U8) -> a
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl @3.1-6.10
		(ty-header @3.1-3.16 (name "Hash")
			(ty-args
				(ty-var @3.6-3.7 (name "a"))
				(ty-var @3.9-3.15 (name "hasher"))))
		(ty-var @3.19-3.20 (name "a")))
	(s-alias-decl @8.1-8.53
		(ty-header @8.1-8.10 (name "Decode")
			(ty-args
				(ty-var @8.8-8.9 (name "a"))))
		(ty-var @8.13-8.14 (name "a"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias @3.1-6.10 (type "Hash(a, hasher)")
			(ty-header @3.1-3.16 (name "Hash")
				(ty-args
					(ty-var @3.6-3.7 (name "a"))
					(ty-var @3.9-3.15 (name "hasher")))))
		(alias @8.1-8.53 (type "Decode(a)")
			(ty-header @8.1-8.10 (name "Decode")
				(ty-args
					(ty-var @8.8-8.9 (name "a"))))))
	(expressions))
~~~

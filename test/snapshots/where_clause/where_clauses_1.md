# META
~~~ini
description=where_clauses (1)
type=file
~~~
# SOURCE
~~~roc
Hash(a, hasher) : a
	where
		module(a).hash : hasher -> hasher,
		module(hasher).Hasher

Decode(a) : a where module(a).decode : List(U8) -> a
~~~
# EXPECTED
WHERE CLAUSE IN TYPE DECLARATION - where_clauses_1.md:2:2:4:24
WHERE CLAUSE IN TYPE DECLARATION - where_clauses_1.md:6:15:6:53
TYPE MODULE MISSING MATCHING TYPE - where_clauses_1.md:1:1:6:53
# PROBLEMS
**WHERE CLAUSE IN TYPE DECLARATION**
Type declarations cannot include `where` clauses.

Only type annotations (such as annottions for a function or other value) can have them.

**where_clauses_1.md:2:2:4:24:**
```roc
	where
		module(a).hash : hasher -> hasher,
		module(hasher).Hasher
```


**WHERE CLAUSE IN TYPE DECLARATION**
Type declarations cannot include `where` clauses.

Only type annotations (such as annottions for a function or other value) can have them.

**where_clauses_1.md:6:15:6:53:**
```roc
Decode(a) : a where module(a).decode : List(U8) -> a
```
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**TYPE MODULE MISSING MATCHING TYPE**
Type modules must have a type declaration matching the module name.

This file is named `where_clauses_1.roc`, but no top-level type declaration named `where_clauses_1` was found.

Add either:
`where_clauses_1 := ...` (nominal type)
or:
`where_clauses_1 : ...` (type alias)
**where_clauses_1.md:1:1:6:53:**
```roc
Hash(a, hasher) : a
	where
		module(a).hash : hasher -> hasher,
		module(hasher).Hasher

Decode(a) : a where module(a).decode : List(U8) -> a
```


# TOKENS
~~~zig
UpperIdent(1:1-1:5),NoSpaceOpenRound(1:5-1:6),LowerIdent(1:6-1:7),Comma(1:7-1:8),LowerIdent(1:9-1:15),CloseRound(1:15-1:16),OpColon(1:17-1:18),LowerIdent(1:19-1:20),
KwWhere(2:2-2:7),
KwModule(3:3-3:9),NoSpaceOpenRound(3:9-3:10),LowerIdent(3:10-3:11),CloseRound(3:11-3:12),NoSpaceDotLowerIdent(3:12-3:17),OpColon(3:18-3:19),LowerIdent(3:20-3:26),OpArrow(3:27-3:29),LowerIdent(3:30-3:36),Comma(3:36-3:37),
KwModule(4:3-4:9),NoSpaceOpenRound(4:9-4:10),LowerIdent(4:10-4:16),CloseRound(4:16-4:17),NoSpaceDotUpperIdent(4:17-4:24),
UpperIdent(6:1-6:7),NoSpaceOpenRound(6:7-6:8),LowerIdent(6:8-6:9),CloseRound(6:9-6:10),OpColon(6:11-6:12),LowerIdent(6:13-6:14),KwWhere(6:15-6:20),KwModule(6:21-6:27),NoSpaceOpenRound(6:27-6:28),LowerIdent(6:28-6:29),CloseRound(6:29-6:30),NoSpaceDotLowerIdent(6:30-6:37),OpColon(6:38-6:39),UpperIdent(6:40-6:44),NoSpaceOpenRound(6:44-6:45),UpperIdent(6:45-6:47),CloseRound(6:47-6:48),OpArrow(6:49-6:51),LowerIdent(6:52-6:53),
EndOfFile(7:1-7:1),
~~~
# PARSE
~~~clojure
(file @1.1-6.53
	(type-module @1.1-1.5)
	(statements
		(s-type-decl @1.1-4.24
			(header @1.1-1.16 (name "Hash")
				(args
					(ty-var @1.6-1.7 (raw "a"))
					(ty-var @1.9-1.15 (raw "hasher"))))
			(ty-var @1.19-1.20 (raw "a")))
		(s-type-decl @6.1-6.53
			(header @6.1-6.10 (name "Decode")
				(args
					(ty-var @6.8-6.9 (raw "a"))))
			(ty-var @6.13-6.14 (raw "a")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl @1.1-4.24
		(ty-header @1.1-1.16 (name "Hash")
			(ty-args
				(ty-var @1.6-1.7 (name "a"))
				(ty-var @1.9-1.15 (name "hasher"))))
		(ty-var @1.19-1.20 (name "a")))
	(s-alias-decl @6.1-6.53
		(ty-header @6.1-6.10 (name "Decode")
			(ty-args
				(ty-var @6.8-6.9 (name "a"))))
		(ty-var @6.13-6.14 (name "a"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias @1.1-4.24 (type "Hash(a, hasher)")
			(ty-header @1.1-1.16 (name "Hash")
				(ty-args
					(ty-var @1.6-1.7 (name "a"))
					(ty-var @1.9-1.15 (name "hasher")))))
		(alias @6.1-6.53 (type "Decode(a)")
			(ty-header @6.1-6.10 (name "Decode")
				(ty-args
					(ty-var @6.8-6.9 (name "a"))))))
	(expressions))
~~~

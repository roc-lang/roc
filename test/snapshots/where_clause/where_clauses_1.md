# META
~~~ini
description=where_clauses (1)
type=file:WhereClauses1.roc
~~~
# SOURCE
~~~roc
WhereClauses1 := {}

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
**WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION**
You cannot define a `where` clause inside a type declaration.

You're attempting do this here:
**where_clauses_1.md:3:1:6:24:**
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
UpperIdent(1:1-1:14),OpColonEqual(1:15-1:17),OpenCurly(1:18-1:19),CloseCurly(1:19-1:20),
UpperIdent(3:1-3:5),NoSpaceOpenRound(3:5-3:6),LowerIdent(3:6-3:7),Comma(3:7-3:8),LowerIdent(3:9-3:15),CloseRound(3:15-3:16),OpColon(3:17-3:18),LowerIdent(3:19-3:20),
KwWhere(4:2-4:7),
KwModule(5:3-5:9),NoSpaceOpenRound(5:9-5:10),LowerIdent(5:10-5:11),CloseRound(5:11-5:12),NoSpaceDotLowerIdent(5:12-5:17),OpColon(5:18-5:19),LowerIdent(5:20-5:26),OpArrow(5:27-5:29),LowerIdent(5:30-5:36),Comma(5:36-5:37),
KwModule(6:3-6:9),NoSpaceOpenRound(6:9-6:10),LowerIdent(6:10-6:16),CloseRound(6:16-6:17),NoSpaceDotUpperIdent(6:17-6:24),
UpperIdent(8:1-8:7),NoSpaceOpenRound(8:7-8:8),LowerIdent(8:8-8:9),CloseRound(8:9-8:10),OpColon(8:11-8:12),LowerIdent(8:13-8:14),KwWhere(8:15-8:20),KwModule(8:21-8:27),NoSpaceOpenRound(8:27-8:28),LowerIdent(8:28-8:29),CloseRound(8:29-8:30),NoSpaceDotLowerIdent(8:30-8:37),OpColon(8:38-8:39),UpperIdent(8:40-8:44),NoSpaceOpenRound(8:44-8:45),UpperIdent(8:45-8:47),CloseRound(8:47-8:48),OpArrow(8:49-8:51),LowerIdent(8:52-8:53),
EndOfFile(9:1-9:1),
~~~
# PARSE
~~~clojure
(file @1.1-8.53
	(type-module @1.1-1.14)
	(statements
		(s-type-decl @1.1-1.20
			(header @1.1-1.14 (name "WhereClauses1")
				(args))
			(ty-record @1.18-1.20))
		(s-type-decl @3.1-6.24
			(header @3.1-3.16 (name "Hash")
				(args
					(ty-var @3.6-3.7 (raw "a"))
					(ty-var @3.9-3.15 (raw "hasher"))))
			(ty-var @3.19-3.20 (raw "a")))
		(s-type-decl @8.1-8.53
			(header @8.1-8.10 (name "Decode")
				(args
					(ty-var @8.8-8.9 (raw "a"))))
			(ty-var @8.13-8.14 (raw "a")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-nominal-decl @1.1-1.20
		(ty-header @1.1-1.14 (name "WhereClauses1"))
		(ty-record @1.18-1.20))
	(s-alias-decl @3.1-6.24
		(ty-header @3.1-3.16 (name "Hash")
			(ty-args
				(ty-rigid-var @3.6-3.7 (name "a"))
				(ty-rigid-var @3.9-3.15 (name "hasher"))))
		(ty-rigid-var-lookup (ty-rigid-var @3.6-3.7 (name "a"))))
	(s-alias-decl @8.1-8.53
		(ty-header @8.1-8.10 (name "Decode")
			(ty-args
				(ty-rigid-var @8.8-8.9 (name "a"))))
		(ty-rigid-var-lookup (ty-rigid-var @8.8-8.9 (name "a")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(nominal @1.1-1.20 (type "WhereClauses1")
			(ty-header @1.1-1.14 (name "WhereClauses1")))
		(alias @3.1-6.24 (type "Hash(a, hasher)")
			(ty-header @3.1-3.16 (name "Hash")
				(ty-args
					(ty-rigid-var @3.6-3.7 (name "a"))
					(ty-rigid-var @3.9-3.15 (name "hasher")))))
		(alias @8.1-8.53 (type "Decode(a)")
			(ty-header @8.1-8.10 (name "Decode")
				(ty-args
					(ty-rigid-var @8.8-8.9 (name "a"))))))
	(expressions))
~~~

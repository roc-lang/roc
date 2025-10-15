# META
~~~ini
description=where_clauses (1)
type=snippet
~~~
# SOURCE
~~~roc
Hash(a, hasher) : a
	where [a.hash : hasher -> hasher, hasher.Hasher]

Decode(a) : a where [a.decode : List(U8) -> a]
~~~
# EXPECTED
WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION - where_clauses_1.md:1:1:2:50
WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION - where_clauses_1.md:4:1:4:47
# PROBLEMS
**WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION**
You cannot define a `where` clause inside a type declaration.

You're attempting do this here:
**where_clauses_1.md:1:1:2:50:**
```roc
Hash(a, hasher) : a
	where [a.hash : hasher -> hasher, hasher.Hasher]
```


**WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION**
You cannot define a `where` clause inside a type declaration.

You're attempting do this here:
**where_clauses_1.md:4:1:4:47:**
```roc
Decode(a) : a where [a.decode : List(U8) -> a]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
UpperIdent(1:1-1:5),NoSpaceOpenRound(1:5-1:6),LowerIdent(1:6-1:7),Comma(1:7-1:8),LowerIdent(1:9-1:15),CloseRound(1:15-1:16),OpColon(1:17-1:18),LowerIdent(1:19-1:20),
KwWhere(2:2-2:7),OpenSquare(2:8-2:9),LowerIdent(2:9-2:10),NoSpaceDotLowerIdent(2:10-2:15),OpColon(2:16-2:17),LowerIdent(2:18-2:24),OpArrow(2:25-2:27),LowerIdent(2:28-2:34),Comma(2:34-2:35),LowerIdent(2:36-2:42),NoSpaceDotUpperIdent(2:42-2:49),CloseSquare(2:49-2:50),
UpperIdent(4:1-4:7),NoSpaceOpenRound(4:7-4:8),LowerIdent(4:8-4:9),CloseRound(4:9-4:10),OpColon(4:11-4:12),LowerIdent(4:13-4:14),KwWhere(4:15-4:20),OpenSquare(4:21-4:22),LowerIdent(4:22-4:23),NoSpaceDotLowerIdent(4:23-4:30),OpColon(4:31-4:32),UpperIdent(4:33-4:37),NoSpaceOpenRound(4:37-4:38),UpperIdent(4:38-4:40),CloseRound(4:40-4:41),OpArrow(4:42-4:44),LowerIdent(4:45-4:46),CloseSquare(4:46-4:47),
EndOfFile(5:1-5:1),
~~~
# PARSE
~~~clojure
(file @1.1-4.47
	(type-module @1.1-1.5)
	(statements
		(s-type-decl @1.1-2.50
			(header @1.1-1.16 (name "Hash")
				(args
					(ty-var @1.6-1.7 (raw "a"))
					(ty-var @1.9-1.15 (raw "hasher"))))
			(ty-var @1.19-1.20 (raw "a")))
		(s-type-decl @4.1-4.47
			(header @4.1-4.10 (name "Decode")
				(args
					(ty-var @4.8-4.9 (raw "a"))))
			(ty-var @4.13-4.14 (raw "a")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl @1.1-2.50
		(ty-header @1.1-1.16 (name "Hash")
			(ty-args
				(ty-rigid-var @1.6-1.7 (name "a"))
				(ty-rigid-var @1.9-1.15 (name "hasher"))))
		(ty-rigid-var-lookup (ty-rigid-var @1.6-1.7 (name "a"))))
	(s-alias-decl @4.1-4.47
		(ty-header @4.1-4.10 (name "Decode")
			(ty-args
				(ty-rigid-var @4.8-4.9 (name "a"))))
		(ty-rigid-var-lookup (ty-rigid-var @4.8-4.9 (name "a")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias @1.1-2.50 (type "Hash(a, hasher)")
			(ty-header @1.1-1.16 (name "Hash")
				(ty-args
					(ty-rigid-var @1.6-1.7 (name "a"))
					(ty-rigid-var @1.9-1.15 (name "hasher")))))
		(alias @4.1-4.47 (type "Decode(a)")
			(ty-header @4.1-4.10 (name "Decode")
				(ty-args
					(ty-rigid-var @4.8-4.9 (name "a"))))))
	(expressions))
~~~

# META
~~~ini
description=where_clauses (7)
type=snippet
~~~
# SOURCE
~~~roc
Hash(a, hasher) # After header
	: # After colon
		a # After var
			where # After where
				module(a).hash : hasher # After method
					-> # After arrow
						hasher, # After first clause
				module(hasher).Hasher

Decode(a) : a
	where
		module(a).decode( # After method args open
			List(U8), # After method arg
		) -> a
~~~
# EXPECTED
WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION - where_clauses_7.md:1:1:8:26
WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION - where_clauses_7.md:10:1:14:9
# PROBLEMS
**WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION**
You cannot define a `where` clause inside a type declaration.

You're attempting do this here:
**where_clauses_7.md:1:1:8:26:**
```roc
Hash(a, hasher) # After header
	: # After colon
		a # After var
			where # After where
				module(a).hash : hasher # After method
					-> # After arrow
						hasher, # After first clause
				module(hasher).Hasher
```


**WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION**
You cannot define a `where` clause inside a type declaration.

You're attempting do this here:
**where_clauses_7.md:10:1:14:9:**
```roc
Decode(a) : a
	where
		module(a).decode( # After method args open
			List(U8), # After method arg
		) -> a
```


# TOKENS
~~~zig
UpperIdent(1:1-1:5),NoSpaceOpenRound(1:5-1:6),LowerIdent(1:6-1:7),Comma(1:7-1:8),LowerIdent(1:9-1:15),CloseRound(1:15-1:16),
OpColon(2:2-2:3),
LowerIdent(3:3-3:4),
KwWhere(4:4-4:9),
KwModule(5:5-5:11),NoSpaceOpenRound(5:11-5:12),LowerIdent(5:12-5:13),CloseRound(5:13-5:14),NoSpaceDotLowerIdent(5:14-5:19),OpColon(5:20-5:21),LowerIdent(5:22-5:28),
OpArrow(6:6-6:8),
LowerIdent(7:7-7:13),Comma(7:13-7:14),
KwModule(8:5-8:11),NoSpaceOpenRound(8:11-8:12),LowerIdent(8:12-8:18),CloseRound(8:18-8:19),NoSpaceDotUpperIdent(8:19-8:26),
UpperIdent(10:1-10:7),NoSpaceOpenRound(10:7-10:8),LowerIdent(10:8-10:9),CloseRound(10:9-10:10),OpColon(10:11-10:12),LowerIdent(10:13-10:14),
KwWhere(11:2-11:7),
KwModule(12:3-12:9),NoSpaceOpenRound(12:9-12:10),LowerIdent(12:10-12:11),CloseRound(12:11-12:12),NoSpaceDotLowerIdent(12:12-12:19),NoSpaceOpenRound(12:19-12:20),
UpperIdent(13:4-13:8),NoSpaceOpenRound(13:8-13:9),UpperIdent(13:9-13:11),CloseRound(13:11-13:12),Comma(13:12-13:13),
CloseRound(14:3-14:4),OpArrow(14:5-14:7),LowerIdent(14:8-14:9),
EndOfFile(15:1-15:1),
~~~
# PARSE
~~~clojure
(file @1.1-14.9
	(type-module @1.1-1.5)
	(statements
		(s-type-decl @1.1-8.26
			(header @1.1-1.16 (name "Hash")
				(args
					(ty-var @1.6-1.7 (raw "a"))
					(ty-var @1.9-1.15 (raw "hasher"))))
			(ty-var @3.3-3.4 (raw "a")))
		(s-type-decl @10.1-14.9
			(header @10.1-10.10 (name "Decode")
				(args
					(ty-var @10.8-10.9 (raw "a"))))
			(ty-var @10.13-10.14 (raw "a")))))
~~~
# FORMATTED
~~~roc
Hash(a, hasher) # After header
	: # After colon
		a # After var
			where # After where
				module(a).hash : hasher # After method
					-> # After arrow
						hasher, # After first clause
				module(hasher).Hasher

Decode(a) : a
	where
		module(a).decode : List(U8) -> a
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl @1.1-8.26
		(ty-header @1.1-1.16 (name "Hash")
			(ty-args
				(ty-rigid-var @1.6-1.7 (name "a"))
				(ty-rigid-var @1.9-1.15 (name "hasher"))))
		(ty-rigid-var-lookup (ty-rigid-var @1.6-1.7 (name "a"))))
	(s-alias-decl @10.1-14.9
		(ty-header @10.1-10.10 (name "Decode")
			(ty-args
				(ty-rigid-var @10.8-10.9 (name "a"))))
		(ty-rigid-var-lookup (ty-rigid-var @10.8-10.9 (name "a")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias @1.1-8.26 (type "Hash(a, hasher)")
			(ty-header @1.1-1.16 (name "Hash")
				(ty-args
					(ty-rigid-var @1.6-1.7 (name "a"))
					(ty-rigid-var @1.9-1.15 (name "hasher")))))
		(alias @10.1-14.9 (type "Decode(a)")
			(ty-header @10.1-10.10 (name "Decode")
				(ty-args
					(ty-rigid-var @10.8-10.9 (name "a"))))))
	(expressions))
~~~

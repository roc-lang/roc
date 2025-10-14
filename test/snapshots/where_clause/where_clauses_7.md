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
			where [ # After where
				a.hash : hasher # After method
					-> # After arrow
						hasher, # After first clause
				hasher.Hasher]

Decode(a) : a
	where [a.decode : List(U8) -> a]
~~~
# EXPECTED
WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION - where_clauses_7.md:1:1:8:19
WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION - where_clauses_7.md:10:1:11:34
# PROBLEMS
**WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION**
You cannot define a `where` clause inside a type declaration.

You're attempting do this here:
**where_clauses_7.md:1:1:8:19:**
```roc
Hash(a, hasher) # After header
	: # After colon
		a # After var
			where [ # After where
				a.hash : hasher # After method
					-> # After arrow
						hasher, # After first clause
				hasher.Hasher]
```


**WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION**
You cannot define a `where` clause inside a type declaration.

You're attempting do this here:
**where_clauses_7.md:10:1:11:34:**
```roc
Decode(a) : a
	where [a.decode : List(U8) -> a]
```


# TOKENS
~~~zig
UpperIdent(1:1-1:5),NoSpaceOpenRound(1:5-1:6),LowerIdent(1:6-1:7),Comma(1:7-1:8),LowerIdent(1:9-1:15),CloseRound(1:15-1:16),
OpColon(2:2-2:3),
LowerIdent(3:3-3:4),
KwWhere(4:4-4:9),OpenSquare(4:10-4:11),
LowerIdent(5:5-5:6),NoSpaceDotLowerIdent(5:6-5:11),OpColon(5:12-5:13),LowerIdent(5:14-5:20),
OpArrow(6:6-6:8),
LowerIdent(7:7-7:13),Comma(7:13-7:14),
LowerIdent(8:5-8:11),NoSpaceDotUpperIdent(8:11-8:18),CloseSquare(8:18-8:19),
UpperIdent(10:1-10:7),NoSpaceOpenRound(10:7-10:8),LowerIdent(10:8-10:9),CloseRound(10:9-10:10),OpColon(10:11-10:12),LowerIdent(10:13-10:14),
KwWhere(11:2-11:7),OpenSquare(11:8-11:9),LowerIdent(11:9-11:10),NoSpaceDotLowerIdent(11:10-11:17),OpColon(11:18-11:19),UpperIdent(11:20-11:24),NoSpaceOpenRound(11:24-11:25),UpperIdent(11:25-11:27),CloseRound(11:27-11:28),OpArrow(11:29-11:31),LowerIdent(11:32-11:33),CloseSquare(11:33-11:34),
EndOfFile(12:1-12:1),
~~~
# PARSE
~~~clojure
(file @1.1-11.34
	(type-module @1.1-1.5)
	(statements
		(s-type-decl @1.1-8.19
			(header @1.1-1.16 (name "Hash")
				(args
					(ty-var @1.6-1.7 (raw "a"))
					(ty-var @1.9-1.15 (raw "hasher"))))
			(ty-var @3.3-3.4 (raw "a")))
		(s-type-decl @10.1-11.34
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
			where
				[ # After where
				# After where
					a.hash : hasher # After method
					-> # After arrow
						hasher # After first clause
,
				hasher.Hasher]

Decode(a) : a
	where [a.decode : List(U8) -> a]
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl @1.1-8.19
		(ty-header @1.1-1.16 (name "Hash")
			(ty-args
				(ty-rigid-var @1.6-1.7 (name "a"))
				(ty-rigid-var @1.9-1.15 (name "hasher"))))
		(ty-rigid-var-lookup (ty-rigid-var @1.6-1.7 (name "a"))))
	(s-alias-decl @10.1-11.34
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
		(alias @1.1-8.19 (type "Hash(a, hasher)")
			(ty-header @1.1-1.16 (name "Hash")
				(ty-args
					(ty-rigid-var @1.6-1.7 (name "a"))
					(ty-rigid-var @1.9-1.15 (name "hasher")))))
		(alias @10.1-11.34 (type "Decode(a)")
			(ty-header @10.1-10.10 (name "Decode")
				(ty-args
					(ty-rigid-var @10.8-10.9 (name "a"))))))
	(expressions))
~~~

# META
~~~ini
description=where_clauses (7)
type=file
~~~
# SOURCE
~~~roc
module [Hash]

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
WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION - where_clauses_7.md:3:1:10:26
WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION - where_clauses_7.md:12:1:16:9
# PROBLEMS
**WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION**
You cannot define a `where` clause inside a type declaration.

You're attempting do this here:
**where_clauses_7.md:3:1:10:26:**
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
**where_clauses_7.md:12:1:16:9:**
```roc
Decode(a) : a
	where
		module(a).decode( # After method args open
			List(U8), # After method arg
		) -> a
```


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:13),CloseSquare(1:13-1:14),
UpperIdent(3:1-3:5),NoSpaceOpenRound(3:5-3:6),LowerIdent(3:6-3:7),Comma(3:7-3:8),LowerIdent(3:9-3:15),CloseRound(3:15-3:16),
OpColon(4:2-4:3),
LowerIdent(5:3-5:4),
KwWhere(6:4-6:9),
KwModule(7:5-7:11),NoSpaceOpenRound(7:11-7:12),LowerIdent(7:12-7:13),CloseRound(7:13-7:14),NoSpaceDotLowerIdent(7:14-7:19),OpColon(7:20-7:21),LowerIdent(7:22-7:28),
OpArrow(8:6-8:8),
LowerIdent(9:7-9:13),Comma(9:13-9:14),
KwModule(10:5-10:11),NoSpaceOpenRound(10:11-10:12),LowerIdent(10:12-10:18),CloseRound(10:18-10:19),NoSpaceDotUpperIdent(10:19-10:26),
UpperIdent(12:1-12:7),NoSpaceOpenRound(12:7-12:8),LowerIdent(12:8-12:9),CloseRound(12:9-12:10),OpColon(12:11-12:12),LowerIdent(12:13-12:14),
KwWhere(13:2-13:7),
KwModule(14:3-14:9),NoSpaceOpenRound(14:9-14:10),LowerIdent(14:10-14:11),CloseRound(14:11-14:12),NoSpaceDotLowerIdent(14:12-14:19),NoSpaceOpenRound(14:19-14:20),
UpperIdent(15:4-15:8),NoSpaceOpenRound(15:8-15:9),UpperIdent(15:9-15:11),CloseRound(15:11-15:12),Comma(15:12-15:13),
CloseRound(16:3-16:4),OpArrow(16:5-16:7),LowerIdent(16:8-16:9),
EndOfFile(17:1-17:1),
~~~
# PARSE
~~~clojure
(file @1.1-16.9
	(module @1.1-1.14
		(exposes @1.8-1.14
			(exposed-upper-ident @1.9-1.13 (text "Hash"))))
	(statements
		(s-type-decl @3.1-10.26
			(header @3.1-3.16 (name "Hash")
				(args
					(ty-var @3.6-3.7 (raw "a"))
					(ty-var @3.9-3.15 (raw "hasher"))))
			(ty-var @5.3-5.4 (raw "a"))
			(where
				(method @7.5-9.13 (module-of "a") (name "hash")
					(args
						(ty-var @7.22-7.28 (raw "hasher")))
					(ty-var @9.7-9.13 (raw "hasher")))
				(alias @10.5-10.26 (module-of "hasher") (name "Hasher"))))
		(s-type-decl @12.1-16.9
			(header @12.1-12.10 (name "Decode")
				(args
					(ty-var @12.8-12.9 (raw "a"))))
			(ty-var @12.13-12.14 (raw "a"))
			(where
				(method @14.3-16.9 (module-of "a") (name "decode")
					(args
						(ty-apply @15.4-15.12
							(ty @15.4-15.8 (name "List"))
							(ty @15.9-15.11 (name "U8"))))
					(ty-var @16.8-16.9 (raw "a")))))))
~~~
# FORMATTED
~~~roc
module [Hash]

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
	(s-alias-decl @3.1-10.26
		(ty-header @3.1-3.16 (name "Hash")
			(ty-args
				(ty-var @3.6-3.7 (name "a"))
				(ty-var @3.9-3.15 (name "hasher"))))
		(ty-var @5.3-5.4 (name "a")))
	(s-alias-decl @12.1-16.9
		(ty-header @12.1-12.10 (name "Decode")
			(ty-args
				(ty-var @12.8-12.9 (name "a"))))
		(ty-var @12.13-12.14 (name "a"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias @3.1-10.26 (type "Hash(a, hasher)")
			(ty-header @3.1-3.16 (name "Hash")
				(ty-args
					(ty-var @3.6-3.7 (name "a"))
					(ty-var @3.9-3.15 (name "hasher")))))
		(alias @12.1-16.9 (type "Decode(a)")
			(ty-header @12.1-12.10 (name "Decode")
				(ty-args
					(ty-var @12.8-12.9 (name "a"))))))
	(expressions))
~~~

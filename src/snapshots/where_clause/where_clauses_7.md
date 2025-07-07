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
				module(hasher).Hasher,

Decode(a) : a
	where
		module(a).decode( # After method args open
			List(U8), # After method arg
		) -> a,
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:13),CloseSquare(1:13-1:14),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(3:1-3:5),NoSpaceOpenRound(3:5-3:6),LowerIdent(3:6-3:7),Comma(3:7-3:8),LowerIdent(3:9-3:15),CloseRound(3:15-3:16),Newline(3:18-3:31),
OpColon(4:2-4:3),Newline(4:5-4:17),
LowerIdent(5:3-5:4),Newline(5:6-5:16),
KwWhere(6:4-6:9),Newline(6:11-6:23),
KwModule(7:5-7:11),NoSpaceOpenRound(7:11-7:12),LowerIdent(7:12-7:13),CloseRound(7:13-7:14),NoSpaceDotLowerIdent(7:14-7:19),OpColon(7:20-7:21),LowerIdent(7:22-7:28),Newline(7:30-7:43),
OpArrow(8:6-8:8),Newline(8:10-8:22),
LowerIdent(9:7-9:13),Comma(9:13-9:14),Newline(9:16-9:35),
KwModule(10:5-10:11),NoSpaceOpenRound(10:11-10:12),LowerIdent(10:12-10:18),CloseRound(10:18-10:19),NoSpaceDotUpperIdent(10:19-10:26),Comma(10:26-10:27),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(12:1-12:7),NoSpaceOpenRound(12:7-12:8),LowerIdent(12:8-12:9),CloseRound(12:9-12:10),OpColon(12:11-12:12),LowerIdent(12:13-12:14),Newline(1:1-1:1),
KwWhere(13:2-13:7),Newline(1:1-1:1),
KwModule(14:3-14:9),NoSpaceOpenRound(14:9-14:10),LowerIdent(14:10-14:11),CloseRound(14:11-14:12),NoSpaceDotLowerIdent(14:12-14:19),NoSpaceOpenRound(14:19-14:20),Newline(14:22-14:45),
UpperIdent(15:4-15:8),NoSpaceOpenRound(15:8-15:9),UpperIdent(15:9-15:11),CloseRound(15:11-15:12),Comma(15:12-15:13),Newline(15:15-15:32),
CloseRound(16:3-16:4),OpArrow(16:5-16:7),LowerIdent(16:8-16:9),Comma(16:9-16:10),EndOfFile(16:10-16:10),
~~~
# PARSE
~~~clojure
(file @1.1-16.10
	(module @1.1-1.14
		(exposes @1.8-1.14
			(exposed-upper-ident (text "Hash"))))
	(statements
		(s-type-decl @3.1-12.7
			(header @3.1-3.16 (name "Hash")
				(args
					(ty-var @3.6-3.7 (raw "a"))
					(ty-var @3.9-3.15 (raw "hasher"))))
			(ty-var @5.3-5.4 (raw "a"))
			(where
				(method @7.5-9.14 (module-of "a") (name "hash")
					(args
						(ty-var @7.22-7.28 (raw "hasher")))
					(ty-var @9.7-9.13 (raw "hasher")))
				(alias @10.5-10.27 (module-of "hasher") (name "Hasher"))))
		(s-type-decl @12.1-16.10
			(header @12.1-12.10 (name "Decode")
				(args
					(ty-var @12.8-12.9 (raw "a"))))
			(ty-var @12.13-12.14 (raw "a"))
			(where
				(method @14.3-16.10 (module-of "a") (name "decode")
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
			module(a).hash : hasher -> hasher, # After first clause
			module(hasher).Hasher,

Decode(a) : a
 where
	module(a).decode : List(U8) -> a,
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl @3.1-12.7
		(ty-header @3.1-3.16 (name "Hash")
			(ty-args
				(ty-var @3.6-3.7 (name "a"))
				(ty-var @3.9-3.15 (name "hasher"))))
		(ty-var @5.3-5.4 (name "a"))
		(where
			(where-method @7.5-9.14 (module-of "a") (function "hash")
				(args
					(ty-var @7.22-7.28 (name "hasher")))
				(ty-var @9.7-9.13 (name "hasher")))
			(where-alias @10.5-10.27 (module-of "hasher") (alias "Hasher"))))
	(s-alias-decl @12.1-16.10
		(ty-header @12.1-12.10 (name "Decode")
			(ty-args
				(ty-var @12.8-12.9 (name "a"))))
		(ty-var @12.13-12.14 (name "a"))
		(where
			(where-method @14.3-16.10 (module-of "a") (function "decode")
				(args
					(ty-apply @15.4-15.12 (symbol "List")
						(ty @15.9-15.11 (name "U8"))))
				(ty-var @16.8-16.9 (name "a"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias @3.1-12.7 (type "Hash(a, hasher)")
			(ty-header @3.1-3.16 (name "Hash")
				(ty-args
					(ty-var @3.6-3.7 (name "a"))
					(ty-var @3.9-3.15 (name "hasher")))))
		(alias @12.1-16.10 (type "Decode(a)")
			(ty-header @12.1-12.10 (name "Decode")
				(ty-args
					(ty-var @12.8-12.9 (name "a"))))))
	(expressions))
~~~

# META
~~~ini
description=where_clauses (7)
type=file
~~~
# SOURCE
~~~roc
module [Hash]

Hash(a) # After header
	: # After colon
		a # After var
			where # After where
				a.hash(hasher) # After method
					-> # After arrow
						hasher, # After first clause
				hasher.Hasher,

Decode(a) : a
	where
		module(a).decode( # After method args open
			List(U8), # After method arg
		) -> a,
~~~
# PROBLEMS
**NOT IMPLEMENTED**
This feature is not yet implemented: top-level type_decl
**NOT IMPLEMENTED**
This feature is not yet implemented: top-level type_decl
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:13),CloseSquare(1:13-1:14),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(3:1-3:5),NoSpaceOpenRound(3:5-3:6),LowerIdent(3:6-3:7),CloseRound(3:7-3:8),Newline(3:10-3:23),
OpColon(4:2-4:3),Newline(4:5-4:17),
LowerIdent(5:3-5:4),Newline(5:6-5:16),
KwWhere(6:4-6:9),Newline(6:11-6:23),
LowerIdent(7:5-7:6),NoSpaceDotLowerIdent(7:6-7:11),NoSpaceOpenRound(7:11-7:12),LowerIdent(7:12-7:18),CloseRound(7:18-7:19),Newline(7:21-7:34),
OpArrow(8:6-8:8),Newline(8:10-8:22),
LowerIdent(9:7-9:13),Comma(9:13-9:14),Newline(9:16-9:35),
LowerIdent(10:5-10:11),NoSpaceDotUpperIdent(10:11-10:18),Comma(10:18-10:19),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(12:1-12:7),NoSpaceOpenRound(12:7-12:8),LowerIdent(12:8-12:9),CloseRound(12:9-12:10),OpColon(12:11-12:12),LowerIdent(12:13-12:14),Newline(1:1-1:1),
KwWhere(13:2-13:7),Newline(1:1-1:1),
KwModule(14:3-14:9),NoSpaceOpenRound(14:9-14:10),LowerIdent(14:10-14:11),CloseRound(14:11-14:12),NoSpaceDotLowerIdent(14:12-14:19),NoSpaceOpenRound(14:19-14:20),Newline(14:22-14:45),
UpperIdent(15:4-15:8),NoSpaceOpenRound(15:8-15:9),UpperIdent(15:9-15:11),CloseRound(15:11-15:12),Comma(15:12-15:13),Newline(15:15-15:32),
CloseRound(16:3-16:4),OpArrow(16:5-16:7),LowerIdent(16:8-16:9),Comma(16:9-16:10),EndOfFile(16:10-16:10),
~~~
# PARSE
~~~clojure
(file (1:1-16:10)
	(module (1:1-1:14)
		(exposes (1:8-1:14) (exposed_item (upper_ident "Hash"))))
	(statements
		(type_decl (3:1-12:7)
			(header (3:1-3:8)
				"Hash"
				(args (ty_var (3:6-3:7) "a")))
			(ty_var (5:3-5:4) "a"))
		(type_decl (12:1-16:10)
			(header (12:1-12:10)
				"Decode"
				(args (ty_var (12:8-12:9) "a")))
			(ty_var (12:13-12:14) "a"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can_ir "empty")
~~~
# TYPES
~~~clojure
(inferred_types (defs) (expressions))
~~~
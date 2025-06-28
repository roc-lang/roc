# META
~~~ini
description=where_clauses (1)
type=file
~~~
# SOURCE
~~~roc
module [Hash]

Hash(a) : a
	where
		a.hash(hasher) -> hasher,
		hasher.Hasher,

Decode(a) : a
	where
		module(a).decode(List(U8)) -> a,
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:13),CloseSquare(1:13-1:14),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(3:1-3:5),NoSpaceOpenRound(3:5-3:6),LowerIdent(3:6-3:7),CloseRound(3:7-3:8),OpColon(3:9-3:10),LowerIdent(3:11-3:12),Newline(1:1-1:1),
KwWhere(4:2-4:7),Newline(1:1-1:1),
LowerIdent(5:3-5:4),NoSpaceDotLowerIdent(5:4-5:9),NoSpaceOpenRound(5:9-5:10),LowerIdent(5:10-5:16),CloseRound(5:16-5:17),OpArrow(5:18-5:20),LowerIdent(5:21-5:27),Comma(5:27-5:28),Newline(1:1-1:1),
LowerIdent(6:3-6:9),NoSpaceDotUpperIdent(6:9-6:16),Comma(6:16-6:17),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(8:1-8:7),NoSpaceOpenRound(8:7-8:8),LowerIdent(8:8-8:9),CloseRound(8:9-8:10),OpColon(8:11-8:12),LowerIdent(8:13-8:14),Newline(1:1-1:1),
KwWhere(9:2-9:7),Newline(1:1-1:1),
KwModule(10:3-10:9),NoSpaceOpenRound(10:9-10:10),LowerIdent(10:10-10:11),CloseRound(10:11-10:12),NoSpaceDotLowerIdent(10:12-10:19),NoSpaceOpenRound(10:19-10:20),UpperIdent(10:20-10:24),NoSpaceOpenRound(10:24-10:25),UpperIdent(10:25-10:27),CloseRound(10:27-10:28),CloseRound(10:28-10:29),OpArrow(10:30-10:32),LowerIdent(10:33-10:34),Comma(10:34-10:35),EndOfFile(10:35-10:35),
~~~
# PARSE
~~~clojure
(file @1-1-10-35
	(module @1-1-1-14
		(exposes @1-8-1-14
			(exposed-upper-ident (text "Hash"))))
	(statements
		(s-type-decl @3-1-8-7
			(header @3-1-3-8 (name "Hash")
				(args
					(ty-var @3-6-3-7 (raw "a"))))
			(ty-var @3-11-3-12 (raw "a")))
		(s-type-decl @8-1-10-35
			(header @8-1-8-10 (name "Decode")
				(args
					(ty-var @8-8-8-9 (raw "a"))))
			(ty-var @8-13-8-14 (raw "a")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-type-decl @3-1-8-7 (id 75)
		(ty-header @3-1-3-8 (name "Hash")
			(ty-args
				(ty-var @3-6-3-7 (name "a"))))
		(ty-var @3-11-3-12 (name "a")))
	(s-type-decl @8-1-10-35 (id 79)
		(ty-header @8-1-8-10 (name "Decode")
			(ty-args
				(ty-var @8-8-8-9 (name "a"))))
		(ty-var @8-13-8-14 (name "a"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~

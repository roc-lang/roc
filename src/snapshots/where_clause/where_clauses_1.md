# META
~~~ini
description=where_clauses (1)
type=file
~~~
# SOURCE
~~~roc
module [Hash, Decode]

Hash(a) : a
	where
		module(a).hash(hasher) -> hasher,
		module(hasher).Hasher,

Decode(a) : a where module(a).decode : List(U8) -> a
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:13),Comma(1:13-1:14),UpperIdent(1:15-1:21),CloseSquare(1:21-1:22),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(3:1-3:5),NoSpaceOpenRound(3:5-3:6),LowerIdent(3:6-3:7),CloseRound(3:7-3:8),OpColon(3:9-3:10),LowerIdent(3:11-3:12),Newline(1:1-1:1),
KwWhere(4:2-4:7),Newline(1:1-1:1),
KwModule(5:3-5:9),NoSpaceOpenRound(5:9-5:10),LowerIdent(5:10-5:11),CloseRound(5:11-5:12),NoSpaceDotLowerIdent(5:12-5:17),NoSpaceOpenRound(5:17-5:18),LowerIdent(5:18-5:24),CloseRound(5:24-5:25),OpArrow(5:26-5:28),LowerIdent(5:29-5:35),Comma(5:35-5:36),Newline(1:1-1:1),
KwModule(6:3-6:9),NoSpaceOpenRound(6:9-6:10),LowerIdent(6:10-6:16),CloseRound(6:16-6:17),NoSpaceDotUpperIdent(6:17-6:24),Comma(6:24-6:25),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(8:1-8:7),NoSpaceOpenRound(8:7-8:8),LowerIdent(8:8-8:9),CloseRound(8:9-8:10),OpColon(8:11-8:12),LowerIdent(8:13-8:14),KwWhere(8:15-8:20),KwModule(8:21-8:27),NoSpaceOpenRound(8:27-8:28),LowerIdent(8:28-8:29),CloseRound(8:29-8:30),NoSpaceDotLowerIdent(8:30-8:37),OpColon(8:38-8:39),UpperIdent(8:40-8:44),NoSpaceOpenRound(8:44-8:45),UpperIdent(8:45-8:47),CloseRound(8:47-8:48),OpArrow(8:49-8:51),LowerIdent(8:52-8:53),EndOfFile(8:53-8:53),
~~~
# PARSE
~~~clojure
(file @1.1-8.53
	(module @1.1-1.22
		(exposes @1.8-1.22
			(exposed-upper-ident (text "Hash"))
			(exposed-upper-ident (text "Decode"))))
	(statements
		(s-type-decl @3.1-8.7
			(header @3.1-3.8 (name "Hash")
				(args
					(ty-var @3.6-3.7 (raw "a"))))
			(ty-var @3.11-3.12 (raw "a"))
			(where
				(method @5.3-5.36 (module-of "a") (name "hash")
					(args
						(ty-var @5.18-5.24 (raw "hasher")))
					(ty-var @5.29-5.35 (raw "hasher")))
				(alias @6.3-6.25 (module-of "hasher") (name "Hasher"))))
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

Hash(a) : a
 where
	module(a).hash : hasher -> hasher,
	module(hasher).Hasher,

Decode(a) : a where module(a).decode : List(U8) -> a
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl @3.1-8.7
		(ty-header @3.1-3.8 (name "Hash")
			(ty-args
				(ty-var @3.6-3.7 (name "a"))))
		(ty-var @3.11-3.12 (name "a")))
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
		(alias @3.1-8.7 (type "Hash(a)")
			(ty-header @3.1-3.8 (name "Hash")
				(ty-args
					(ty-var @3.6-3.7 (name "a")))))
		(alias @8.1-8.53 (type "Decode(a)")
			(ty-header @8.1-8.10 (name "Decode")
				(ty-args
					(ty-var @8.8-8.9 (name "a"))))))
	(expressions))
~~~

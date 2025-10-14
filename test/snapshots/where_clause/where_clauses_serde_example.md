# META
~~~ini
description=Module dispatch in where clause
type=snippet
~~~
# SOURCE
~~~roc
deserialize : List(U8) -> Result(a, [DecodeErr])
	where [a.decode : List(U8) -> Result(a, [DecodeErr])]
deserialize = |_| ...
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:12),OpColon(1:13-1:14),UpperIdent(1:15-1:19),NoSpaceOpenRound(1:19-1:20),UpperIdent(1:20-1:22),CloseRound(1:22-1:23),OpArrow(1:24-1:26),UpperIdent(1:27-1:33),NoSpaceOpenRound(1:33-1:34),LowerIdent(1:34-1:35),Comma(1:35-1:36),OpenSquare(1:37-1:38),UpperIdent(1:38-1:47),CloseSquare(1:47-1:48),CloseRound(1:48-1:49),
KwWhere(2:2-2:7),OpenSquare(2:8-2:9),LowerIdent(2:9-2:10),NoSpaceDotLowerIdent(2:10-2:17),OpColon(2:18-2:19),UpperIdent(2:20-2:24),NoSpaceOpenRound(2:24-2:25),UpperIdent(2:25-2:27),CloseRound(2:27-2:28),OpArrow(2:29-2:31),UpperIdent(2:32-2:38),NoSpaceOpenRound(2:38-2:39),LowerIdent(2:39-2:40),Comma(2:40-2:41),OpenSquare(2:42-2:43),UpperIdent(2:43-2:52),CloseSquare(2:52-2:53),CloseRound(2:53-2:54),CloseSquare(2:54-2:55),
LowerIdent(3:1-3:12),OpAssign(3:13-3:14),OpBar(3:15-3:16),Underscore(3:16-3:17),OpBar(3:17-3:18),TripleDot(3:19-3:22),
EndOfFile(4:1-4:1),
~~~
# PARSE
~~~clojure
(file @1.1-3.22
	(type-module @1.1-1.12)
	(statements
		(s-type-anno @1.1-2.55 (name "deserialize")
			(ty-fn @1.15-1.49
				(ty-apply @1.15-1.23
					(ty @1.15-1.19 (name "List"))
					(ty @1.20-1.22 (name "U8")))
				(ty-apply @1.27-1.49
					(ty @1.27-1.33 (name "Result"))
					(ty-var @1.34-1.35 (raw "a"))
					(ty-tag-union @1.37-1.48
						(tags
							(ty @1.38-1.47 (name "DecodeErr"))))))
			(where
				(method @2.9-2.54 (module-of "a") (name "decode")
					(args
						(ty-apply @2.20-2.28
							(ty @2.20-2.24 (name "List"))
							(ty @2.25-2.27 (name "U8"))))
					(ty-apply @2.32-2.54
						(ty @2.32-2.38 (name "Result"))
						(ty-var @2.39-2.40 (raw "a"))
						(ty-tag-union @2.42-2.53
							(tags
								(ty @2.43-2.52 (name "DecodeErr"))))))))
		(s-decl @3.1-3.22
			(p-ident @3.1-3.12 (raw "deserialize"))
			(e-lambda @3.15-3.22
				(args
					(p-underscore))
				(e-ellipsis)))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @3.1-3.12 (ident "deserialize"))
		(e-lambda @3.15-3.22
			(args
				(p-underscore @3.16-3.17))
			(e-not-implemented @1.1-1.1))
		(annotation @3.1-3.12
			(declared-type
				(ty-fn @1.15-1.49 (effectful false)
					(ty-apply @1.15-1.23 (name "List") (builtin)
						(ty-lookup @1.20-1.22 (name "U8") (builtin)))
					(ty-apply @1.27-1.49 (name "Result") (local)
						(ty-rigid-var @1.27-1.49 (name "a"))
						(ty-tag-union @1.27-1.49
							(ty-tag-name @1.38-1.47 (name "DecodeErr"))))))))
	(s-type-anno @1.1-2.55 (name "deserialize")
		(ty-fn @1.15-1.49 (effectful false)
			(ty-apply @1.15-1.23 (name "List") (builtin)
				(ty-lookup @1.20-1.22 (name "U8") (builtin)))
			(ty-apply @1.27-1.49 (name "Result") (local)
				(ty-rigid-var @1.27-1.49 (name "a"))
				(ty-tag-union @1.27-1.49
					(ty-tag-name @1.38-1.47 (name "DecodeErr")))))
		(where
			(method @2.9-2.54 (module-of "a") (ident "decode")
				(args
					(ty-apply @2.20-2.28 (name "List") (builtin)
						(ty-lookup @2.25-2.27 (name "U8") (builtin))))
				(ty-apply @2.32-2.54 (name "Result") (local)
					(ty-rigid-var-lookup (ty-rigid-var @1.27-1.49 (name "a")))
					(ty-tag-union @2.42-2.53
						(ty-tag-name @2.43-2.52 (name "DecodeErr")))))))
	(ext-decl @2.9-2.54 (ident "a.decode") (kind "value")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.12 (type "List(Num(Int(Unsigned8))) -> Result(a, [DecodeErr])")))
	(expressions
		(expr @3.15-3.22 (type "List(Num(Int(Unsigned8))) -> Result(a, [DecodeErr])"))))
~~~

# META
~~~ini
description=Module dispatch in where clause
type=file
~~~
# SOURCE
~~~roc
module [deserialize]

deserialize : List(U8) -> Result(a, [DecodeErr])
  where module(a).decode : List(U8) -> Result(a, [DecodeErr])
deserialize = |_| ...
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:20),CloseSquare(1:20-1:21),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:12),OpColon(3:13-3:14),UpperIdent(3:15-3:19),NoSpaceOpenRound(3:19-3:20),UpperIdent(3:20-3:22),CloseRound(3:22-3:23),OpArrow(3:24-3:26),UpperIdent(3:27-3:33),NoSpaceOpenRound(3:33-3:34),LowerIdent(3:34-3:35),Comma(3:35-3:36),OpenSquare(3:37-3:38),UpperIdent(3:38-3:47),CloseSquare(3:47-3:48),CloseRound(3:48-3:49),Newline(1:1-1:1),
KwWhere(4:3-4:8),KwModule(4:9-4:15),NoSpaceOpenRound(4:15-4:16),LowerIdent(4:16-4:17),CloseRound(4:17-4:18),NoSpaceDotLowerIdent(4:18-4:25),OpColon(4:26-4:27),UpperIdent(4:28-4:32),NoSpaceOpenRound(4:32-4:33),UpperIdent(4:33-4:35),CloseRound(4:35-4:36),OpArrow(4:37-4:39),UpperIdent(4:40-4:46),NoSpaceOpenRound(4:46-4:47),LowerIdent(4:47-4:48),Comma(4:48-4:49),OpenSquare(4:50-4:51),UpperIdent(4:51-4:60),CloseSquare(4:60-4:61),CloseRound(4:61-4:62),Newline(1:1-1:1),
LowerIdent(5:1-5:12),OpAssign(5:13-5:14),OpBar(5:15-5:16),Underscore(5:16-5:17),OpBar(5:17-5:18),TripleDot(5:19-5:22),EndOfFile(5:22-5:22),
~~~
# PARSE
~~~clojure
(file @1.1-5.22
	(module @1.1-1.21
		(exposes @1.8-1.21
			(exposed-lower-ident (text "deserialize"))))
	(statements
		(s-type-anno @3.1-5.12 (name "deserialize")
			(ty-fn @3.15-3.49
				(ty-apply @3.15-3.23
					(ty @3.15-3.19 (name "List"))
					(ty @3.20-3.22 (name "U8")))
				(ty-apply @3.27-3.49
					(ty @3.27-3.33 (name "Result"))
					(ty-var @3.34-3.35 (raw "a"))
					(ty-tag-union @3.37-3.48
						(tags
							(ty @3.38-3.47 (name "DecodeErr"))))))
			(where
				(method @4.9-5.12 (module-of "a") (name "decode")
					(args
						(ty-apply @4.28-4.36
							(ty @4.28-4.32 (name "List"))
							(ty @4.33-4.35 (name "U8"))))
					(ty-apply @4.40-4.62
						(ty @4.40-4.46 (name "Result"))
						(ty-var @4.47-4.48 (raw "a"))
						(ty-tag-union @4.50-4.61
							(tags
								(ty @4.51-4.60 (name "DecodeErr"))))))))
		(s-decl @5.1-5.22
			(p-ident @5.1-5.12 (raw "deserialize"))
			(e-lambda @5.15-5.22
				(args
					(p-underscore))
				(e-ellipsis)))))
~~~
# FORMATTED
~~~roc
module [deserialize]

deserialize : List(U8) -> Result(a, [DecodeErr])
 where module(a).decode : List(U8) -> Result(a, [DecodeErr])
deserialize = |_| ...
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @5.1-5.12 (ident "deserialize"))
		(e-lambda @5.15-5.22
			(args
				(p-underscore @5.16-5.17))
			(e-not-implemented))
		(annotation @5.1-5.12
			(declared-type
				(ty-fn @3.15-3.49 (effectful false)
					(ty-apply @3.15-3.23 (symbol "List")
						(ty @3.20-3.22 (name "U8")))
					(ty-apply @3.27-3.49 (symbol "Result")
						(ty-var @3.34-3.35 (name "a"))
						(ty-tag-union @3.37-3.48
							(ty @3.38-3.47 (name "DecodeErr"))))))))
	(s-type-anno @3.1-5.12 (name "deserialize")
		(ty-fn @3.15-3.49 (effectful false)
			(ty-apply @3.15-3.23 (symbol "List")
				(ty @3.20-3.22 (name "U8")))
			(ty-apply @3.27-3.49 (symbol "Result")
				(ty-var @3.34-3.35 (name "a"))
				(ty-tag-union @3.37-3.48
					(ty @3.38-3.47 (name "DecodeErr")))))
		(where
			(method @4.9-5.12 (module-of "a") (ident "decode")
				(args
					(ty-apply @4.28-4.36 (symbol "List")
						(ty @4.33-4.35 (name "U8"))))
				(ty-apply @4.40-4.62 (symbol "Result")
					(ty-var @4.47-4.48 (name "a"))
					(ty-tag-union @4.50-4.61
						(ty @4.51-4.60 (name "DecodeErr"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @5.1-5.12 (type "Error -> Error")))
	(expressions
		(expr @5.15-5.22 (type "Error -> Error"))))
~~~

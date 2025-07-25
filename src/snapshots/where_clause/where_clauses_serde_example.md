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
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:20),CloseSquare(1:20-1:21),
LowerIdent(3:1-3:12),OpColon(3:13-3:14),UpperIdent(3:15-3:19),NoSpaceOpenRound(3:19-3:20),UpperIdent(3:20-3:22),CloseRound(3:22-3:23),OpArrow(3:24-3:26),UpperIdent(3:27-3:33),NoSpaceOpenRound(3:33-3:34),LowerIdent(3:34-3:35),Comma(3:35-3:36),OpenSquare(3:37-3:38),UpperIdent(3:38-3:47),CloseSquare(3:47-3:48),CloseRound(3:48-3:49),
KwWhere(4:2-4:7),KwModule(4:8-4:14),NoSpaceOpenRound(4:14-4:15),LowerIdent(4:15-4:16),CloseRound(4:16-4:17),NoSpaceDotLowerIdent(4:17-4:24),OpColon(4:25-4:26),UpperIdent(4:27-4:31),NoSpaceOpenRound(4:31-4:32),UpperIdent(4:32-4:34),CloseRound(4:34-4:35),OpArrow(4:36-4:38),UpperIdent(4:39-4:45),NoSpaceOpenRound(4:45-4:46),LowerIdent(4:46-4:47),Comma(4:47-4:48),OpenSquare(4:49-4:50),UpperIdent(4:50-4:59),CloseSquare(4:59-4:60),CloseRound(4:60-4:61),
LowerIdent(5:1-5:12),OpAssign(5:13-5:14),OpBar(5:15-5:16),Underscore(5:16-5:17),OpBar(5:17-5:18),TripleDot(5:19-5:22),EndOfFile(5:22-5:22),
~~~
# PARSE
~~~clojure
(file @1.1-5.22
	(module @1.1-1.21
		(exposes @1.8-1.21
			(exposed-lower-ident @1.9-1.20
				(text "deserialize"))))
	(statements
		(s-type-anno @3.1-4.61 (name "deserialize")
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
				(method @4.8-4.61 (module-of "a") (name "decode")
					(args
						(ty-apply @4.27-4.35
							(ty @4.27-4.31 (name "List"))
							(ty @4.32-4.34 (name "U8"))))
					(ty-apply @4.39-4.61
						(ty @4.39-4.45 (name "Result"))
						(ty-var @4.46-4.47 (raw "a"))
						(ty-tag-union @4.49-4.60
							(tags
								(ty @4.50-4.59 (name "DecodeErr"))))))))
		(s-decl @5.1-5.22
			(p-ident @5.1-5.12 (raw "deserialize"))
			(e-lambda @5.15-5.22
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
		(p-assign @5.1-5.12 (ident "deserialize"))
		(e-closure @5.15-5.22
			(e-lambda @5.15-5.22
				(args
					(p-underscore @5.16-5.17))
				(e-not-implemented @1.1-1.1)))
		(annotation @5.1-5.12
			(declared-type
				(ty-fn @3.15-3.49 (effectful false)
					(ty-apply @3.15-3.23 (symbol "List")
						(ty @3.20-3.22 (name "U8")))
					(ty-apply @3.27-3.49 (symbol "Result")
						(ty-var @3.34-3.35 (name "a"))
						(ty-tag-union @3.37-3.48
							(ty @3.38-3.47 (name "DecodeErr"))))))))
	(s-type-anno @3.1-4.61 (name "deserialize")
		(ty-fn @3.15-3.49 (effectful false)
			(ty-apply @3.15-3.23 (symbol "List")
				(ty @3.20-3.22 (name "U8")))
			(ty-apply @3.27-3.49 (symbol "Result")
				(ty-var @3.34-3.35 (name "a"))
				(ty-tag-union @3.37-3.48
					(ty @3.38-3.47 (name "DecodeErr")))))
		(where
			(method @4.8-4.61 (module-of "a") (ident "decode")
				(args
					(ty-apply @4.27-4.35 (symbol "List")
						(ty @4.32-4.34 (name "U8"))))
				(ty-apply @4.39-4.61 (symbol "Result")
					(ty-var @4.46-4.47 (name "a"))
					(ty-tag-union @4.49-4.60
						(ty @4.50-4.59 (name "DecodeErr")))))))
	(ext-decl @4.8-4.61 (ident "module(a).decode") (kind "value")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @5.1-5.12 (type "Error -> Error")))
	(expressions
		(expr @5.15-5.22 (type "Error -> Error"))))
~~~

# META
~~~ini
description=Multiple where constraints on different type variables
type=file
~~~
# SOURCE
~~~roc
module [process]

process : a, b -> c where module(a).convert : a -> c, module(b).transform : b -> c
process = |_, _| ...
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:16),CloseSquare(1:16-1:17),
LowerIdent(3:1-3:8),OpColon(3:9-3:10),LowerIdent(3:11-3:12),Comma(3:12-3:13),LowerIdent(3:14-3:15),OpArrow(3:16-3:18),LowerIdent(3:19-3:20),KwWhere(3:21-3:26),KwModule(3:27-3:33),NoSpaceOpenRound(3:33-3:34),LowerIdent(3:34-3:35),CloseRound(3:35-3:36),NoSpaceDotLowerIdent(3:36-3:44),OpColon(3:45-3:46),LowerIdent(3:47-3:48),OpArrow(3:49-3:51),LowerIdent(3:52-3:53),Comma(3:53-3:54),KwModule(3:55-3:61),NoSpaceOpenRound(3:61-3:62),LowerIdent(3:62-3:63),CloseRound(3:63-3:64),NoSpaceDotLowerIdent(3:64-3:74),OpColon(3:75-3:76),LowerIdent(3:77-3:78),OpArrow(3:79-3:81),LowerIdent(3:82-3:83),
LowerIdent(4:1-4:8),OpAssign(4:9-4:10),OpBar(4:11-4:12),Underscore(4:12-4:13),Comma(4:13-4:14),Underscore(4:15-4:16),OpBar(4:16-4:17),TripleDot(4:18-4:21),EndOfFile(4:21-4:21),
~~~
# PARSE
~~~clojure
(file @1.1-4.21
	(module @1.1-1.17
		(exposes @1.8-1.17
			(exposed-lower-ident @1.9-1.16 (text "process"))))
	(statements
		(s-type-anno @3.1-3.83 (name "process")
			(ty-fn @3.11-3.20
				(ty-var @1.1-1.1 (raw "a"))
				(ty-var @1.1-1.1 (raw "b"))
				(ty-var @1.1-1.1 (raw "c")))
			(where
				(method @3.27-3.53 (module-of "a") (name "convert")
					(args
						(ty-var @1.1-1.1 (raw "a")))
					(ty-var @1.1-1.1 (raw "c")))
				(method @3.55-3.83 (module-of "b") (name "transform")
					(args
						(ty-var @1.1-1.1 (raw "b")))
					(ty-var @1.1-1.1 (raw "c")))))
		(s-decl @4.1-4.21
			(p-ident @4.1-4.8 (raw "process"))
			(e-lambda @4.11-4.21
				(args
					(p-underscore)
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
		(p-assign @4.1-4.8 (ident "process"))
		(e-lambda @4.11-4.21
			(args
				(p-underscore @4.12-4.13)
				(p-underscore @4.15-4.16))
			(e-not-implemented @1.1-1.1))
		(annotation @4.1-4.8
			(declared-type
				(ty-fn @3.11-3.20 (effectful false)
					(ty-var @1.1-1.1 (name "a"))
					(ty-var @1.1-1.1 (name "b"))
					(ty-var @1.1-1.1 (name "c"))))))
	(s-type-anno @3.1-3.83 (name "process")
		(ty-fn @3.11-3.20 (effectful false)
			(ty-var @1.1-1.1 (name "a"))
			(ty-var @1.1-1.1 (name "b"))
			(ty-var @1.1-1.1 (name "c")))
		(where
			(method @3.27-3.53 (module-of "a") (ident "convert")
				(args
					(ty-var @1.1-1.1 (name "a")))
				(ty-var @1.1-1.1 (name "c")))
			(method @3.55-3.83 (module-of "b") (ident "transform")
				(args
					(ty-var @1.1-1.1 (name "b")))
				(ty-var @1.1-1.1 (name "c")))))
	(ext-decl @3.27-3.53 (ident "module(a).convert") (kind "value"))
	(ext-decl @3.55-3.83 (ident "module(b).transform") (kind "value")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.8 (type "*, * -> *")))
	(expressions
		(expr @4.11-4.21 (type "*, * -> *"))))
~~~

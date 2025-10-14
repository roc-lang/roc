# META
~~~ini
description=Multiple where constraints on different type variables
type=snippet
~~~
# SOURCE
~~~roc
process : a, b -> c where module(a).convert : a -> c, module(b).transform : b -> c
process = |_, _| ...
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:8),OpColon(1:9-1:10),LowerIdent(1:11-1:12),Comma(1:12-1:13),LowerIdent(1:14-1:15),OpArrow(1:16-1:18),LowerIdent(1:19-1:20),KwWhere(1:21-1:26),KwModule(1:27-1:33),NoSpaceOpenRound(1:33-1:34),LowerIdent(1:34-1:35),CloseRound(1:35-1:36),NoSpaceDotLowerIdent(1:36-1:44),OpColon(1:45-1:46),LowerIdent(1:47-1:48),OpArrow(1:49-1:51),LowerIdent(1:52-1:53),Comma(1:53-1:54),KwModule(1:55-1:61),NoSpaceOpenRound(1:61-1:62),LowerIdent(1:62-1:63),CloseRound(1:63-1:64),NoSpaceDotLowerIdent(1:64-1:74),OpColon(1:75-1:76),LowerIdent(1:77-1:78),OpArrow(1:79-1:81),LowerIdent(1:82-1:83),
LowerIdent(2:1-2:8),OpAssign(2:9-2:10),OpBar(2:11-2:12),Underscore(2:12-2:13),Comma(2:13-2:14),Underscore(2:15-2:16),OpBar(2:16-2:17),TripleDot(2:18-2:21),
EndOfFile(3:1-3:1),
~~~
# PARSE
~~~clojure
(file @1.1-2.21
	(type-module @1.1-1.8)
	(statements
		(s-type-anno @1.1-1.83 (name "process")
			(ty-fn @1.11-1.20
				(ty-var @1.11-1.12 (raw "a"))
				(ty-var @1.14-1.15 (raw "b"))
				(ty-var @1.19-1.20 (raw "c")))
			(where
				(method @1.27-1.53 (module-of "a") (name "convert")
					(args
						(ty-var @1.47-1.48 (raw "a")))
					(ty-var @1.52-1.53 (raw "c")))
				(method @1.55-1.83 (module-of "b") (name "transform")
					(args
						(ty-var @1.77-1.78 (raw "b")))
					(ty-var @1.82-1.83 (raw "c")))))
		(s-decl @2.1-2.21
			(p-ident @2.1-2.8 (raw "process"))
			(e-lambda @2.11-2.21
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
		(p-assign @2.1-2.8 (ident "process"))
		(e-lambda @2.11-2.21
			(args
				(p-underscore @2.12-2.13)
				(p-underscore @2.15-2.16))
			(e-not-implemented @1.1-1.1))
		(annotation @2.1-2.8
			(declared-type
				(ty-fn @1.11-1.20 (effectful false)
					(ty-rigid-var @1.11-1.12 (name "a"))
					(ty-rigid-var @1.14-1.15 (name "b"))
					(ty-rigid-var @1.19-1.20 (name "c"))))))
	(s-type-anno @1.1-1.83 (name "process")
		(ty-fn @1.11-1.20 (effectful false)
			(ty-rigid-var @1.11-1.12 (name "a"))
			(ty-rigid-var @1.14-1.15 (name "b"))
			(ty-rigid-var @1.19-1.20 (name "c")))
		(where
			(method @1.27-1.53 (module-of "a") (ident "convert")
				(args
					(ty-rigid-var-lookup (ty-rigid-var @1.11-1.12 (name "a"))))
				(ty-rigid-var-lookup (ty-rigid-var @1.19-1.20 (name "c"))))
			(method @1.55-1.83 (module-of "b") (ident "transform")
				(args
					(ty-rigid-var-lookup (ty-rigid-var @1.14-1.15 (name "b"))))
				(ty-rigid-var-lookup (ty-rigid-var @1.19-1.20 (name "c"))))))
	(ext-decl @1.27-1.53 (ident "module(a).convert") (kind "value"))
	(ext-decl @1.55-1.83 (ident "module(b).transform") (kind "value")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @2.1-2.8 (type "a, b -> c")))
	(expressions
		(expr @2.11-2.21 (type "a, b -> c"))))
~~~

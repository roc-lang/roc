# META
~~~ini
description=Multiple where constraints on different type variables
type=snippet
~~~
# SOURCE
~~~roc
process : a, b -> c where [a.convert : a -> c, b.transform : b -> c]
process = |_, _| ...
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:8),OpColon(1:9-1:10),LowerIdent(1:11-1:12),Comma(1:12-1:13),LowerIdent(1:14-1:15),OpArrow(1:16-1:18),LowerIdent(1:19-1:20),KwWhere(1:21-1:26),OpenSquare(1:27-1:28),LowerIdent(1:28-1:29),NoSpaceDotLowerIdent(1:29-1:37),OpColon(1:38-1:39),LowerIdent(1:40-1:41),OpArrow(1:42-1:44),LowerIdent(1:45-1:46),Comma(1:46-1:47),LowerIdent(1:48-1:49),NoSpaceDotLowerIdent(1:49-1:59),OpColon(1:60-1:61),LowerIdent(1:62-1:63),OpArrow(1:64-1:66),LowerIdent(1:67-1:68),CloseSquare(1:68-1:69),
LowerIdent(2:1-2:8),OpAssign(2:9-2:10),OpBar(2:11-2:12),Underscore(2:12-2:13),Comma(2:13-2:14),Underscore(2:15-2:16),OpBar(2:16-2:17),TripleDot(2:18-2:21),
EndOfFile(3:1-3:1),
~~~
# PARSE
~~~clojure
(file @1.1-2.21
	(type-module @1.1-1.8)
	(statements
		(s-type-anno @1.1-1.69 (name "process")
			(ty-fn @1.11-1.20
				(ty-var @1.11-1.12 (raw "a"))
				(ty-var @1.14-1.15 (raw "b"))
				(ty-var @1.19-1.20 (raw "c")))
			(where
				(method @1.28-1.46 (module-of "a") (name "convert")
					(args
						(ty-var @1.40-1.41 (raw "a")))
					(ty-var @1.45-1.46 (raw "c")))
				(method @1.48-1.68 (module-of "b") (name "transform")
					(args
						(ty-var @1.62-1.63 (raw "b")))
					(ty-var @1.67-1.68 (raw "c")))))
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
	(s-type-anno @1.1-1.69 (name "process")
		(ty-fn @1.11-1.20 (effectful false)
			(ty-rigid-var @1.11-1.12 (name "a"))
			(ty-rigid-var @1.14-1.15 (name "b"))
			(ty-rigid-var @1.19-1.20 (name "c")))
		(where
			(method @1.28-1.46 (module-of "a") (ident "convert")
				(args
					(ty-rigid-var-lookup (ty-rigid-var @1.11-1.12 (name "a"))))
				(ty-rigid-var-lookup (ty-rigid-var @1.19-1.20 (name "c"))))
			(method @1.48-1.68 (module-of "b") (ident "transform")
				(args
					(ty-rigid-var-lookup (ty-rigid-var @1.14-1.15 (name "b"))))
				(ty-rigid-var-lookup (ty-rigid-var @1.19-1.20 (name "c"))))))
	(ext-decl @1.28-1.46 (ident "a.convert") (kind "value"))
	(ext-decl @1.48-1.68 (ident "b.transform") (kind "value")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @2.1-2.8 (type "a, b -> c")))
	(expressions
		(expr @2.11-2.21 (type "a, b -> c"))))
~~~

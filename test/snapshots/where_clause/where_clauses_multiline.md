# META
~~~ini
description=Where clause with multiline constraints
type=snippet
~~~
# SOURCE
~~~roc
process : a, b -> c
	where [a.convert : a -> c, b.transform : b -> c]
process = ...
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:8),OpColon(1:9-1:10),LowerIdent(1:11-1:12),Comma(1:12-1:13),LowerIdent(1:14-1:15),OpArrow(1:16-1:18),LowerIdent(1:19-1:20),
KwWhere(2:2-2:7),OpenSquare(2:8-2:9),LowerIdent(2:9-2:10),NoSpaceDotLowerIdent(2:10-2:18),OpColon(2:19-2:20),LowerIdent(2:21-2:22),OpArrow(2:23-2:25),LowerIdent(2:26-2:27),Comma(2:27-2:28),LowerIdent(2:29-2:30),NoSpaceDotLowerIdent(2:30-2:40),OpColon(2:41-2:42),LowerIdent(2:43-2:44),OpArrow(2:45-2:47),LowerIdent(2:48-2:49),CloseSquare(2:49-2:50),
LowerIdent(3:1-3:8),OpAssign(3:9-3:10),TripleDot(3:11-3:14),
EndOfFile(4:1-4:1),
~~~
# PARSE
~~~clojure
(file @1.1-3.14
	(type-module @1.1-1.8)
	(statements
		(s-type-anno @1.1-2.50 (name "process")
			(ty-fn @1.11-1.20
				(ty-var @1.11-1.12 (raw "a"))
				(ty-var @1.14-1.15 (raw "b"))
				(ty-var @1.19-1.20 (raw "c")))
			(where
				(method @2.9-2.27 (module-of "a") (name "convert")
					(args
						(ty-var @2.21-2.22 (raw "a")))
					(ty-var @2.26-2.27 (raw "c")))
				(method @2.29-2.49 (module-of "b") (name "transform")
					(args
						(ty-var @2.43-2.44 (raw "b")))
					(ty-var @2.48-2.49 (raw "c")))))
		(s-decl @3.1-3.14
			(p-ident @3.1-3.8 (raw "process"))
			(e-ellipsis))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @3.1-3.8 (ident "process"))
		(e-not-implemented @1.1-1.1)
		(annotation @3.1-3.8
			(declared-type
				(ty-fn @1.11-1.20 (effectful false)
					(ty-rigid-var @1.11-1.12 (name "a"))
					(ty-rigid-var @1.14-1.15 (name "b"))
					(ty-rigid-var @1.19-1.20 (name "c"))))))
	(s-type-anno @1.1-2.50 (name "process")
		(ty-fn @1.11-1.20 (effectful false)
			(ty-rigid-var @1.11-1.12 (name "a"))
			(ty-rigid-var @1.14-1.15 (name "b"))
			(ty-rigid-var @1.19-1.20 (name "c")))
		(where
			(method @2.9-2.27 (module-of "a") (ident "convert")
				(args
					(ty-rigid-var-lookup (ty-rigid-var @1.11-1.12 (name "a"))))
				(ty-rigid-var-lookup (ty-rigid-var @1.19-1.20 (name "c"))))
			(method @2.29-2.49 (module-of "b") (ident "transform")
				(args
					(ty-rigid-var-lookup (ty-rigid-var @1.14-1.15 (name "b"))))
				(ty-rigid-var-lookup (ty-rigid-var @1.19-1.20 (name "c"))))))
	(ext-decl @2.9-2.27 (ident "a.convert") (kind "value"))
	(ext-decl @2.29-2.49 (ident "b.transform") (kind "value")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.8 (type "a, b -> c")))
	(expressions
		(expr @1.1-1.1 (type "a, b -> c"))))
~~~

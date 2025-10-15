# META
~~~ini
description=Minimal where clause test
type=snippet
~~~
# SOURCE
~~~roc
convert_me : a -> b
	where [a.convert : a -> b]
convert_me = ...
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:11),OpColon(1:12-1:13),LowerIdent(1:14-1:15),OpArrow(1:16-1:18),LowerIdent(1:19-1:20),
KwWhere(2:2-2:7),OpenSquare(2:8-2:9),LowerIdent(2:9-2:10),NoSpaceDotLowerIdent(2:10-2:18),OpColon(2:19-2:20),LowerIdent(2:21-2:22),OpArrow(2:23-2:25),LowerIdent(2:26-2:27),CloseSquare(2:27-2:28),
LowerIdent(3:1-3:11),OpAssign(3:12-3:13),TripleDot(3:14-3:17),
EndOfFile(4:1-4:1),
~~~
# PARSE
~~~clojure
(file @1.1-3.17
	(type-module @1.1-1.11)
	(statements
		(s-type-anno @1.1-2.28 (name "convert_me")
			(ty-fn @1.14-1.20
				(ty-var @1.14-1.15 (raw "a"))
				(ty-var @1.19-1.20 (raw "b")))
			(where
				(method @2.9-2.27 (module-of "a") (name "convert")
					(args
						(ty-var @2.21-2.22 (raw "a")))
					(ty-var @2.26-2.27 (raw "b")))))
		(s-decl @3.1-3.17
			(p-ident @3.1-3.11 (raw "convert_me"))
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
		(p-assign @3.1-3.11 (ident "convert_me"))
		(e-not-implemented @1.1-1.1)
		(annotation @3.1-3.11
			(declared-type
				(ty-fn @1.14-1.20 (effectful false)
					(ty-rigid-var @1.14-1.15 (name "a"))
					(ty-rigid-var @1.19-1.20 (name "b"))))))
	(s-type-anno @1.1-2.28 (name "convert_me")
		(ty-fn @1.14-1.20 (effectful false)
			(ty-rigid-var @1.14-1.15 (name "a"))
			(ty-rigid-var @1.19-1.20 (name "b")))
		(where
			(method @2.9-2.27 (module-of "a") (ident "convert")
				(args
					(ty-rigid-var-lookup (ty-rigid-var @1.14-1.15 (name "a"))))
				(ty-rigid-var-lookup (ty-rigid-var @1.19-1.20 (name "b"))))))
	(ext-decl @2.9-2.27 (ident "a.convert") (kind "value")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.11 (type "a -> b")))
	(expressions
		(expr @1.1-1.1 (type "a -> b"))))
~~~

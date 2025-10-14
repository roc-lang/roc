# META
~~~ini
description=Simple type annotation with where clause
type=snippet
~~~
# SOURCE
~~~roc
convert : a -> b where [a.to_b : a -> b]
convert = |a| a.to_b()
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:8),OpColon(1:9-1:10),LowerIdent(1:11-1:12),OpArrow(1:13-1:15),LowerIdent(1:16-1:17),KwWhere(1:18-1:23),OpenSquare(1:24-1:25),LowerIdent(1:25-1:26),NoSpaceDotLowerIdent(1:26-1:31),OpColon(1:32-1:33),LowerIdent(1:34-1:35),OpArrow(1:36-1:38),LowerIdent(1:39-1:40),CloseSquare(1:40-1:41),
LowerIdent(2:1-2:8),OpAssign(2:9-2:10),OpBar(2:11-2:12),LowerIdent(2:12-2:13),OpBar(2:13-2:14),LowerIdent(2:15-2:16),NoSpaceDotLowerIdent(2:16-2:21),NoSpaceOpenRound(2:21-2:22),CloseRound(2:22-2:23),
EndOfFile(3:1-3:1),
~~~
# PARSE
~~~clojure
(file @1.1-2.23
	(type-module @1.1-1.8)
	(statements
		(s-type-anno @1.1-1.41 (name "convert")
			(ty-fn @1.11-1.17
				(ty-var @1.11-1.12 (raw "a"))
				(ty-var @1.16-1.17 (raw "b")))
			(where
				(method @1.25-1.40 (module-of "a") (name "to_b")
					(args
						(ty-var @1.34-1.35 (raw "a")))
					(ty-var @1.39-1.40 (raw "b")))))
		(s-decl @2.1-2.23
			(p-ident @2.1-2.8 (raw "convert"))
			(e-lambda @2.11-2.23
				(args
					(p-ident @2.12-2.13 (raw "a")))
				(e-field-access @2.15-2.23
					(e-ident @2.15-2.16 (raw "a"))
					(e-apply @2.16-2.23
						(e-ident @2.16-2.21 (raw "to_b"))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @2.1-2.8 (ident "convert"))
		(e-lambda @2.11-2.23
			(args
				(p-assign @2.12-2.13 (ident "a")))
			(e-dot-access @2.15-2.23 (field "to_b")
				(receiver
					(e-lookup-local @2.15-2.16
						(p-assign @2.12-2.13 (ident "a"))))
				(args)))
		(annotation @2.1-2.8
			(declared-type
				(ty-fn @1.11-1.17 (effectful false)
					(ty-rigid-var @1.11-1.12 (name "a"))
					(ty-rigid-var @1.16-1.17 (name "b"))))))
	(s-type-anno @1.1-1.41 (name "convert")
		(ty-fn @1.11-1.17 (effectful false)
			(ty-rigid-var @1.11-1.12 (name "a"))
			(ty-rigid-var @1.16-1.17 (name "b")))
		(where
			(method @1.25-1.40 (module-of "a") (ident "to_b")
				(args
					(ty-rigid-var-lookup (ty-rigid-var @1.11-1.12 (name "a"))))
				(ty-rigid-var-lookup (ty-rigid-var @1.16-1.17 (name "b"))))))
	(ext-decl @1.25-1.40 (ident "a.to_b") (kind "value")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @2.1-2.8 (type "a -> b")))
	(expressions
		(expr @2.11-2.23 (type "a -> b"))))
~~~

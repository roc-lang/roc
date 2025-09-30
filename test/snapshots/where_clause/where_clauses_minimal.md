# META
~~~ini
description=Minimal where clause test
type=file
~~~
# SOURCE
~~~roc
module [convert_me]

convert_me : a -> b
	where
		module(a).convert : a -> b
convert_me = ...
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:19),CloseSquare(1:19-1:20),
LowerIdent(3:1-3:11),OpColon(3:12-3:13),LowerIdent(3:14-3:15),OpArrow(3:16-3:18),LowerIdent(3:19-3:20),
KwWhere(4:2-4:7),
KwModule(5:3-5:9),NoSpaceOpenRound(5:9-5:10),LowerIdent(5:10-5:11),CloseRound(5:11-5:12),NoSpaceDotLowerIdent(5:12-5:20),OpColon(5:21-5:22),LowerIdent(5:23-5:24),OpArrow(5:25-5:27),LowerIdent(5:28-5:29),
LowerIdent(6:1-6:11),OpAssign(6:12-6:13),TripleDot(6:14-6:17),
EndOfFile(7:1-7:1),
~~~
# PARSE
~~~clojure
(file @1.1-6.17
	(module @1.1-1.20
		(exposes @1.8-1.20
			(exposed-lower-ident @1.9-1.19
				(text "convert_me"))))
	(statements
		(s-type-anno @3.1-5.29 (name "convert_me")
			(ty-fn @3.14-3.20
				(ty-var @3.14-3.15 (raw "a"))
				(ty-var @3.19-3.20 (raw "b")))
			(where
				(method @5.3-5.29 (module-of "a") (name "convert")
					(args
						(ty-var @5.23-5.24 (raw "a")))
					(ty-var @5.28-5.29 (raw "b")))))
		(s-decl @6.1-6.17
			(p-ident @6.1-6.11 (raw "convert_me"))
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
		(p-assign @6.1-6.11 (ident "convert_me"))
		(e-not-implemented @1.1-1.1)
		(annotation @6.1-6.11
			(declared-type
				(ty-fn @3.14-3.20 (effectful false)
					(ty-rigid-var @3.14-3.15 (name "a"))
					(ty-rigid-var @3.19-3.20 (name "b"))))))
	(s-type-anno @3.1-5.29 (name "convert_me")
		(ty-fn @3.14-3.20 (effectful false)
			(ty-rigid-var @3.14-3.15 (name "a"))
			(ty-rigid-var @3.19-3.20 (name "b")))
		(where
			(method @5.3-5.29 (module-of "a") (ident "convert")
				(args
					(ty-rigid-var-lookup (ty-rigid-var @3.14-3.15 (name "a"))))
				(ty-rigid-var-lookup (ty-rigid-var @3.19-3.20 (name "b"))))))
	(ext-decl @5.3-5.29 (ident "module(a).convert") (kind "value")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.11 (type "a -> b")))
	(expressions
		(expr @1.1-1.1 (type "a -> b"))))
~~~

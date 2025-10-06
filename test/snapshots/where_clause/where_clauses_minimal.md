# META
~~~ini
description=Minimal where clause test
type=snippet
~~~
# SOURCE
~~~roc
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
LowerIdent(1:1-1:11),OpColon(1:12-1:13),LowerIdent(1:14-1:15),OpArrow(1:16-1:18),LowerIdent(1:19-1:20),
KwWhere(2:2-2:7),
KwModule(3:3-3:9),NoSpaceOpenRound(3:9-3:10),LowerIdent(3:10-3:11),CloseRound(3:11-3:12),NoSpaceDotLowerIdent(3:12-3:20),OpColon(3:21-3:22),LowerIdent(3:23-3:24),OpArrow(3:25-3:27),LowerIdent(3:28-3:29),
LowerIdent(4:1-4:11),OpAssign(4:12-4:13),TripleDot(4:14-4:17),
EndOfFile(5:1-5:1),
~~~
# PARSE
~~~clojure
(file @1.1-4.17
	(type-module @1.1-1.11)
	(statements
		(s-type-anno @1.1-3.29 (name "convert_me")
			(ty-fn @1.14-1.20
				(ty-var @1.14-1.15 (raw "a"))
				(ty-var @1.19-1.20 (raw "b")))
			(where
				(method @3.3-3.29 (module-of "a") (name "convert")
					(args
						(ty-var @3.23-3.24 (raw "a")))
					(ty-var @3.28-3.29 (raw "b")))))
		(s-decl @4.1-4.17
			(p-ident @4.1-4.11 (raw "convert_me"))
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
		(p-assign @4.1-4.11 (ident "convert_me"))
		(e-not-implemented @1.1-1.1)
		(annotation @4.1-4.11
			(declared-type
				(ty-fn @1.14-1.20 (effectful false)
					(ty-rigid-var @1.14-1.15 (name "a"))
					(ty-rigid-var @1.19-1.20 (name "b"))))))
	(s-type-anno @1.1-3.29 (name "convert_me")
		(ty-fn @1.14-1.20 (effectful false)
			(ty-rigid-var @1.14-1.15 (name "a"))
			(ty-rigid-var @1.19-1.20 (name "b")))
		(where
			(method @3.3-3.29 (module-of "a") (ident "convert")
				(args
					(ty-rigid-var-lookup (ty-rigid-var @1.14-1.15 (name "a"))))
				(ty-rigid-var-lookup (ty-rigid-var @1.19-1.20 (name "b"))))))
	(ext-decl @3.3-3.29 (ident "module(a).convert") (kind "value")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.11 (type "a -> b")))
	(expressions
		(expr @1.1-1.1 (type "a -> b"))))
~~~

# META
~~~ini
description=Where clause with multiline constraints
type=file
~~~
# SOURCE
~~~roc
module [process]

process : a, b -> c
 where
	module(a).convert : a -> c,
	module(b).transform : b -> c,
process = ...
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:16),CloseSquare(1:16-1:17),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:8),OpColon(3:9-3:10),LowerIdent(3:11-3:12),Comma(3:12-3:13),LowerIdent(3:14-3:15),OpArrow(3:16-3:18),LowerIdent(3:19-3:20),Newline(1:1-1:1),
KwWhere(4:2-4:7),Newline(1:1-1:1),
KwModule(5:2-5:8),NoSpaceOpenRound(5:8-5:9),LowerIdent(5:9-5:10),CloseRound(5:10-5:11),NoSpaceDotLowerIdent(5:11-5:19),OpColon(5:20-5:21),LowerIdent(5:22-5:23),OpArrow(5:24-5:26),LowerIdent(5:27-5:28),Comma(5:28-5:29),Newline(1:1-1:1),
KwModule(6:2-6:8),NoSpaceOpenRound(6:8-6:9),LowerIdent(6:9-6:10),CloseRound(6:10-6:11),NoSpaceDotLowerIdent(6:11-6:21),OpColon(6:22-6:23),LowerIdent(6:24-6:25),OpArrow(6:26-6:28),LowerIdent(6:29-6:30),Comma(6:30-6:31),Newline(1:1-1:1),
LowerIdent(7:1-7:8),OpAssign(7:9-7:10),TripleDot(7:11-7:14),EndOfFile(7:14-7:14),
~~~
# PARSE
~~~clojure
(file @1.1-7.14
	(module @1.1-1.17
		(exposes @1.8-1.17
			(exposed-lower-ident (text "process"))))
	(statements
		(s-type-anno @3.1-7.8 (name "process")
			(ty-fn @3.11-3.20
				(ty-var @3.11-3.12 (raw "a"))
				(ty-var @3.14-3.15 (raw "b"))
				(ty-var @3.19-3.20 (raw "c")))
			(where
				(method @5.2-5.29 (module-of "a") (name "convert")
					(args
						(ty-var @5.22-5.23 (raw "a")))
					(ty-var @5.27-5.28 (raw "c")))
				(method @6.2-6.31 (module-of "b") (name "transform")
					(args
						(ty-var @6.24-6.25 (raw "b")))
					(ty-var @6.29-6.30 (raw "c")))))
		(s-decl @7.1-7.14
			(p-ident @7.1-7.8 (raw "process"))
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
		(p-assign @7.1-7.8 (ident "process"))
		(e-not-implemented @7.11-7.14)
		(annotation @7.1-7.8
			(declared-type
				(ty-fn @3.11-3.20 (effectful false)
					(ty-var @3.11-3.12 (name "a"))
					(ty-var @3.14-3.15 (name "b"))
					(ty-var @3.19-3.20 (name "c"))))))
	(s-type-anno @3.1-7.8 (name "process")
		(ty-fn @3.11-3.20 (effectful false)
			(ty-var @3.11-3.12 (name "a"))
			(ty-var @3.14-3.15 (name "b"))
			(ty-var @3.19-3.20 (name "c")))
		(where
			(method @5.2-5.29 (module-of "a") (ident "convert")
				(args
					(ty-var @5.22-5.23 (name "a")))
				(ty-var @5.27-5.28 (name "c")))
			(method @6.2-6.31 (module-of "b") (ident "transform")
				(args
					(ty-var @6.24-6.25 (name "b")))
				(ty-var @6.29-6.30 (name "c")))))
	(ext-decl @5.2-5.29 (ident "module(a).convert") (kind "value"))
	(ext-decl @6.2-6.31 (ident "module(b).transform") (kind "value")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @7.1-7.8 (type "a, b -> c")))
	(expressions
		(expr @7.11-7.14 (type "a, b -> c"))))
~~~

# META
~~~ini
description=Unifying two type variables with conflicting return types
type=file
~~~
# SOURCE
~~~roc
module [use_foo]

foo : a -> a where module(a).convert : a -> Str
foo = |x| x

use_foo : b -> b where module(b).convert : b -> I64
use_foo = |x| foo(x)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:16),CloseSquare(1:16-1:17),
LowerIdent(3:1-3:4),OpColon(3:5-3:6),LowerIdent(3:7-3:8),OpArrow(3:9-3:11),LowerIdent(3:12-3:13),KwWhere(3:14-3:19),KwModule(3:20-3:26),NoSpaceOpenRound(3:26-3:27),LowerIdent(3:27-3:28),CloseRound(3:28-3:29),NoSpaceDotLowerIdent(3:29-3:37),OpColon(3:38-3:39),LowerIdent(3:40-3:41),OpArrow(3:42-3:44),UpperIdent(3:45-3:48),
LowerIdent(4:1-4:4),OpAssign(4:5-4:6),OpBar(4:7-4:8),LowerIdent(4:8-4:9),OpBar(4:9-4:10),LowerIdent(4:11-4:12),
LowerIdent(6:1-6:8),OpColon(6:9-6:10),LowerIdent(6:11-6:12),OpArrow(6:13-6:15),LowerIdent(6:16-6:17),KwWhere(6:18-6:23),KwModule(6:24-6:30),NoSpaceOpenRound(6:30-6:31),LowerIdent(6:31-6:32),CloseRound(6:32-6:33),NoSpaceDotLowerIdent(6:33-6:41),OpColon(6:42-6:43),LowerIdent(6:44-6:45),OpArrow(6:46-6:48),UpperIdent(6:49-6:52),
LowerIdent(7:1-7:8),OpAssign(7:9-7:10),OpBar(7:11-7:12),LowerIdent(7:12-7:13),OpBar(7:13-7:14),LowerIdent(7:15-7:18),NoSpaceOpenRound(7:18-7:19),LowerIdent(7:19-7:20),CloseRound(7:20-7:21),
EndOfFile(8:1-8:1),
~~~
# PARSE
~~~clojure
(file @1.1-7.21
	(module @1.1-1.17
		(exposes @1.8-1.17
			(exposed-lower-ident @1.9-1.16
				(text "use_foo"))))
	(statements
		(s-type-anno @3.1-3.48 (name "foo")
			(ty-fn @3.7-3.13
				(ty-var @3.7-3.8 (raw "a"))
				(ty-var @3.12-3.13 (raw "a")))
			(where
				(method @3.20-3.48 (module-of "a") (name "convert")
					(args
						(ty-var @3.40-3.41 (raw "a")))
					(ty @3.45-3.48 (name "Str")))))
		(s-decl @4.1-4.12
			(p-ident @4.1-4.4 (raw "foo"))
			(e-lambda @4.7-4.12
				(args
					(p-ident @4.8-4.9 (raw "x")))
				(e-ident @4.11-4.12 (raw "x"))))
		(s-type-anno @6.1-6.52 (name "use_foo")
			(ty-fn @6.11-6.17
				(ty-var @6.11-6.12 (raw "b"))
				(ty-var @6.16-6.17 (raw "b")))
			(where
				(method @6.24-6.52 (module-of "b") (name "convert")
					(args
						(ty-var @6.44-6.45 (raw "b")))
					(ty @6.49-6.52 (name "I64")))))
		(s-decl @7.1-7.21
			(p-ident @7.1-7.8 (raw "use_foo"))
			(e-lambda @7.11-7.21
				(args
					(p-ident @7.12-7.13 (raw "x")))
				(e-apply @7.15-7.21
					(e-ident @7.15-7.18 (raw "foo"))
					(e-ident @7.19-7.20 (raw "x")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.4 (ident "foo"))
		(e-lambda @4.7-4.12
			(args
				(p-assign @4.8-4.9 (ident "x")))
			(e-lookup-local @4.11-4.12
				(p-assign @4.8-4.9 (ident "x"))))
		(annotation @4.1-4.4
			(declared-type
				(ty-fn @3.7-3.13 (effectful false)
					(ty-rigid-var @3.7-3.8 (name "a"))
					(ty-rigid-var-lookup (ty-rigid-var @3.7-3.8 (name "a")))))
			(where
				(method @3.20-3.48 (module-of "a") (ident "convert")
					(args
						(ty-rigid-var-lookup (ty-rigid-var @3.7-3.8 (name "a"))))
					(ty-lookup @3.45-3.48 (name "Str") (builtin))))))
	(d-let
		(p-assign @7.1-7.8 (ident "use_foo"))
		(e-closure @7.11-7.21
			(captures
				(capture @4.1-4.4 (ident "foo")))
			(e-lambda @7.11-7.21
				(args
					(p-assign @7.12-7.13 (ident "x")))
				(e-call @7.15-7.21
					(e-lookup-local @7.15-7.18
						(p-assign @4.1-4.4 (ident "foo")))
					(e-lookup-local @7.19-7.20
						(p-assign @7.12-7.13 (ident "x"))))))
		(annotation @7.1-7.8
			(declared-type
				(ty-fn @6.11-6.17 (effectful false)
					(ty-rigid-var @6.11-6.12 (name "b"))
					(ty-rigid-var-lookup (ty-rigid-var @6.11-6.12 (name "b")))))
			(where
				(method @6.24-6.52 (module-of "b") (ident "convert")
					(args
						(ty-rigid-var-lookup (ty-rigid-var @6.11-6.12 (name "b"))))
					(ty-lookup @6.49-6.52 (name "I64") (builtin))))))
	(s-type-anno @3.1-3.48 (name "foo")
		(ty-fn @3.7-3.13 (effectful false)
			(ty-rigid-var @3.7-3.8 (name "a"))
			(ty-rigid-var-lookup (ty-rigid-var @3.7-3.8 (name "a"))))
		(where
			(method @3.20-3.48 (module-of "a") (ident "convert")
				(args
					(ty-rigid-var-lookup (ty-rigid-var @3.7-3.8 (name "a"))))
				(ty-lookup @3.45-3.48 (name "Str") (builtin)))))
	(s-type-anno @6.1-6.52 (name "use_foo")
		(ty-fn @6.11-6.17 (effectful false)
			(ty-rigid-var @6.11-6.12 (name "b"))
			(ty-rigid-var-lookup (ty-rigid-var @6.11-6.12 (name "b"))))
		(where
			(method @6.24-6.52 (module-of "b") (ident "convert")
				(args
					(ty-rigid-var-lookup (ty-rigid-var @6.11-6.12 (name "b"))))
				(ty-lookup @6.49-6.52 (name "I64") (builtin)))))
	(ext-decl @3.20-3.48 (ident "module(a).convert") (kind "value"))
	(ext-decl @6.24-6.52 (ident "module(b).convert") (kind "value")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.4 (type "a -> a"))
		(patt @7.1-7.8 (type "b -> b")))
	(expressions
		(expr @4.7-4.12 (type "a -> a"))
		(expr @7.11-7.21 (type "b -> b"))))
~~~

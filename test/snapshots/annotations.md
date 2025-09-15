# META
~~~ini
description=Example of a nominal tag union with a payload
type=file
~~~
# SOURCE
~~~roc
module []

Pair(a) := [Pair(a, a)]


mkPairInvalid : a, b -> Pair(a)
mkPairInvalid = |x, y| Pair.Pair(x, y)
~~~
# EXPECTED
TYPE MISMATCH - annotations.md:18:28:18:28
INVALID NOMINAL TAG - annotations.md:21:22:21:41
INVALID NOMINAL TAG - annotations.md:24:24:24:39
TYPE MISMATCH - annotations.md:28:35:28:35
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
UpperIdent(3:1-3:5),NoSpaceOpenRound(3:5-3:6),LowerIdent(3:6-3:7),CloseRound(3:7-3:8),OpColonEqual(3:9-3:11),OpenSquare(3:12-3:13),UpperIdent(3:13-3:17),NoSpaceOpenRound(3:17-3:18),LowerIdent(3:18-3:19),Comma(3:19-3:20),LowerIdent(3:21-3:22),CloseRound(3:22-3:23),CloseSquare(3:23-3:24),
LowerIdent(6:1-6:14),OpColon(6:15-6:16),LowerIdent(6:17-6:18),Comma(6:18-6:19),LowerIdent(6:20-6:21),OpArrow(6:22-6:24),UpperIdent(6:25-6:29),NoSpaceOpenRound(6:29-6:30),LowerIdent(6:30-6:31),CloseRound(6:31-6:32),
LowerIdent(7:1-7:14),OpAssign(7:15-7:16),OpBar(7:17-7:18),LowerIdent(7:18-7:19),Comma(7:19-7:20),LowerIdent(7:21-7:22),OpBar(7:22-7:23),UpperIdent(7:24-7:28),NoSpaceDotUpperIdent(7:28-7:33),NoSpaceOpenRound(7:33-7:34),LowerIdent(7:34-7:35),Comma(7:35-7:36),LowerIdent(7:37-7:38),CloseRound(7:38-7:39),
EndOfFile(8:1-8:1),
~~~
# PARSE
~~~clojure
(file @1.1-7.39
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-type-decl @3.1-3.24
			(header @3.1-3.8 (name "Pair")
				(args
					(ty-var @3.6-3.7 (raw "a"))))
			(ty-tag-union @3.12-3.24
				(tags
					(ty-apply @3.13-3.23
						(ty @3.13-3.17 (name "Pair"))
						(ty-var @3.18-3.19 (raw "a"))
						(ty-var @3.21-3.22 (raw "a"))))))
		(s-type-anno @6.1-6.32 (name "mkPairInvalid")
			(ty-fn @6.17-6.32
				(ty-var @6.17-6.18 (raw "a"))
				(ty-var @6.20-6.21 (raw "b"))
				(ty-apply @6.25-6.32
					(ty @6.25-6.29 (name "Pair"))
					(ty-var @6.30-6.31 (raw "a")))))
		(s-decl @7.1-7.39
			(p-ident @7.1-7.14 (raw "mkPairInvalid"))
			(e-lambda @7.17-7.39
				(args
					(p-ident @7.18-7.19 (raw "x"))
					(p-ident @7.21-7.22 (raw "y")))
				(e-apply @7.24-7.39
					(e-tag @7.24-7.33 (raw "Pair.Pair"))
					(e-ident @7.34-7.35 (raw "x"))
					(e-ident @7.37-7.38 (raw "y")))))))
~~~
# FORMATTED
~~~roc
module []

Pair(a) := [Pair(a, a)]

mkPairInvalid : a, b -> Pair(a)
mkPairInvalid = |x, y| Pair.Pair(x, y)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @7.1-7.14 (ident "mkPairInvalid"))
		(e-lambda @7.17-7.39
			(args
				(p-assign @7.18-7.19 (ident "x"))
				(p-assign @7.21-7.22 (ident "y")))
			(e-nominal @7.24-7.39 (nominal "Pair")
				(e-tag @7.24-7.39 (name "Pair")
					(args
						(e-lookup-local @7.34-7.35
							(p-assign @7.18-7.19 (ident "x")))
						(e-lookup-local @7.37-7.38
							(p-assign @7.21-7.22 (ident "y")))))))
		(annotation @7.1-7.14
			(declared-type
				(ty-fn @6.17-6.32 (effectful false)
					(ty-rigid-var @6.17-6.18 (name "a"))
					(ty-rigid-var @6.20-6.21 (name "b"))
					(ty-apply @6.25-6.32 (name "Pair") (local)
						(ty-rigid-var @6.17-6.18 (name "a")))))))
	(s-nominal-decl @1.1-1.1
		(ty-header @1.1-1.1 (name "Bool"))
		(ty-tag-union @1.1-1.1
			(tag_name @1.1-1.1 (name "True"))
			(tag_name @1.1-1.1 (name "False"))))
	(s-nominal-decl @1.1-1.1
		(ty-header @1.1-1.1 (name "Result")
			(ty-args
				(ty-rigid-var @1.1-1.1 (name "ok"))
				(ty-rigid-var @1.1-1.1 (name "err"))))
		(ty-tag-union @1.1-1.1
			(tag_name @1.1-1.1 (name "Ok"))
			(tag_name @1.1-1.1 (name "Err"))))
	(s-nominal-decl @3.1-3.24
		(ty-header @3.1-3.8 (name "Pair")
			(ty-args
				(ty-rigid-var @3.6-3.7 (name "a"))))
		(ty-tag-union @3.12-3.24
			(tag_name @3.13-3.23 (name "Pair")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @7.1-7.14 (type "a, b -> Pair(a)")))
	(type_decls
		(nominal @1.1-1.1 (type "Bool")
			(ty-header @1.1-1.1 (name "Bool")))
		(nominal @1.1-1.1 (type "Result(ok, err)")
			(ty-header @1.1-1.1 (name "Result")
				(ty-args
					(ty-rigid-var @1.1-1.1 (name "ok"))
					(ty-rigid-var @1.1-1.1 (name "err")))))
		(nominal @3.1-3.24 (type "Pair(a)")
			(ty-header @3.1-3.8 (name "Pair")
				(ty-args
					(ty-rigid-var @3.6-3.7 (name "a"))))))
	(expressions
		(expr @7.17-7.39 (type "a, b -> Pair(a)"))))
~~~

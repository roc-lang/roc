# META
~~~ini
description=Example of a nominal tag union with a payload
type=file
~~~
# SOURCE
~~~roc
module [Maybe, some1, none1, some2, none2]

Maybe(a) := [Some(a), None]

some1 : a -> Maybe(a)
some1 = |a| Maybe.Some(a)

none1 : Maybe(_a)
none1 = Maybe.None

some2 = |a| Maybe.Some(a)

none2 = Maybe.None
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:14),Comma(1:14-1:15),LowerIdent(1:16-1:21),Comma(1:21-1:22),LowerIdent(1:23-1:28),Comma(1:28-1:29),LowerIdent(1:30-1:35),Comma(1:35-1:36),LowerIdent(1:37-1:42),CloseSquare(1:42-1:43),
UpperIdent(3:1-3:6),NoSpaceOpenRound(3:6-3:7),LowerIdent(3:7-3:8),CloseRound(3:8-3:9),OpColonEqual(3:10-3:12),OpenSquare(3:13-3:14),UpperIdent(3:14-3:18),NoSpaceOpenRound(3:18-3:19),LowerIdent(3:19-3:20),CloseRound(3:20-3:21),Comma(3:21-3:22),UpperIdent(3:23-3:27),CloseSquare(3:27-3:28),
LowerIdent(5:1-5:6),OpColon(5:7-5:8),LowerIdent(5:9-5:10),OpArrow(5:11-5:13),UpperIdent(5:14-5:19),NoSpaceOpenRound(5:19-5:20),LowerIdent(5:20-5:21),CloseRound(5:21-5:22),
LowerIdent(6:1-6:6),OpAssign(6:7-6:8),OpBar(6:9-6:10),LowerIdent(6:10-6:11),OpBar(6:11-6:12),UpperIdent(6:13-6:18),NoSpaceDotUpperIdent(6:18-6:23),NoSpaceOpenRound(6:23-6:24),LowerIdent(6:24-6:25),CloseRound(6:25-6:26),
LowerIdent(8:1-8:6),OpColon(8:7-8:8),UpperIdent(8:9-8:14),NoSpaceOpenRound(8:14-8:15),NamedUnderscore(8:15-8:17),CloseRound(8:17-8:18),
LowerIdent(9:1-9:6),OpAssign(9:7-9:8),UpperIdent(9:9-9:14),NoSpaceDotUpperIdent(9:14-9:19),
LowerIdent(11:1-11:6),OpAssign(11:7-11:8),OpBar(11:9-11:10),LowerIdent(11:10-11:11),OpBar(11:11-11:12),UpperIdent(11:13-11:18),NoSpaceDotUpperIdent(11:18-11:23),NoSpaceOpenRound(11:23-11:24),LowerIdent(11:24-11:25),CloseRound(11:25-11:26),
LowerIdent(13:1-13:6),OpAssign(13:7-13:8),UpperIdent(13:9-13:14),NoSpaceDotUpperIdent(13:14-13:19),EndOfFile(13:19-13:19),
~~~
# PARSE
~~~clojure
(file @1.1-13.19
	(module @1.1-1.43
		(exposes @1.8-1.43
			(exposed-upper-ident @1.9-1.14 (text "Maybe"))
			(exposed-lower-ident @1.16-1.21
				(text "some1"))
			(exposed-lower-ident @1.23-1.28
				(text "none1"))
			(exposed-lower-ident @1.30-1.35
				(text "some2"))
			(exposed-lower-ident @1.37-1.42
				(text "none2"))))
	(statements
		(s-type-decl @3.1-3.28
			(header @3.1-3.9 (name "Maybe")
				(args
					(ty-var @3.7-3.8 (raw "a"))))
			(ty-tag-union @3.13-3.28
				(tags
					(ty-apply @3.14-3.21
						(ty @3.14-3.18 (name "Some"))
						(ty-var @3.19-3.20 (raw "a")))
					(ty @3.23-3.27 (name "None")))))
		(s-type-anno @5.1-5.22 (name "some1")
			(ty-fn @5.9-5.22
				(ty-var @5.9-5.10 (raw "a"))
				(ty-apply @5.14-5.22
					(ty @5.14-5.19 (name "Maybe"))
					(ty-var @5.20-5.21 (raw "a")))))
		(s-decl @6.1-6.26
			(p-ident @6.1-6.6 (raw "some1"))
			(e-lambda @6.9-6.26
				(args
					(p-ident @6.10-6.11 (raw "a")))
				(e-apply @6.13-6.26
					(e-tag @6.13-6.23 (raw "Maybe.Some"))
					(e-ident @6.24-6.25 (raw "a")))))
		(s-type-anno @8.1-8.18 (name "none1")
			(ty-apply @8.9-8.18
				(ty @8.9-8.14 (name "Maybe"))
				(underscore-ty-var @8.15-8.17 (raw "_a"))))
		(s-decl @9.1-9.19
			(p-ident @9.1-9.6 (raw "none1"))
			(e-tag @9.9-9.19 (raw "Maybe.None")))
		(s-decl @11.1-11.26
			(p-ident @11.1-11.6 (raw "some2"))
			(e-lambda @11.9-11.26
				(args
					(p-ident @11.10-11.11 (raw "a")))
				(e-apply @11.13-11.26
					(e-tag @11.13-11.23 (raw "Maybe.Some"))
					(e-ident @11.24-11.25 (raw "a")))))
		(s-decl @13.1-13.19
			(p-ident @13.1-13.6 (raw "none2"))
			(e-tag @13.9-13.19 (raw "Maybe.None")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @6.1-6.6 (ident "some1"))
		(e-lambda @6.9-6.26
			(args
				(p-assign @6.10-6.11 (ident "a")))
			(e-nominal @6.13-6.23 (nominal "Maybe")
				(e-tag @6.13-6.23 (name "Some")
					(args
						(e-lookup-local @6.24-6.25
							(p-assign @6.10-6.11 (ident "a")))))))
		(annotation @6.1-6.6
			(declared-type
				(ty-fn @5.9-5.22 (effectful false)
					(ty-var @5.9-5.10 (name "a"))
					(ty-apply @5.14-5.22 (symbol "Maybe")
						(ty-var @5.20-5.21 (name "a")))))))
	(d-let
		(p-assign @9.1-9.6 (ident "none1"))
		(e-nominal @9.9-9.19 (nominal "Maybe")
			(e-tag @9.9-9.19 (name "None")))
		(annotation @9.1-9.6
			(declared-type
				(ty-apply @8.9-8.18 (symbol "Maybe")
					(ty-var @8.15-8.17 (name "_a"))))))
	(d-let
		(p-assign @11.1-11.6 (ident "some2"))
		(e-lambda @11.9-11.26
			(args
				(p-assign @11.10-11.11 (ident "a")))
			(e-nominal @11.13-11.23 (nominal "Maybe")
				(e-tag @11.13-11.23 (name "Some")
					(args
						(e-lookup-local @11.24-11.25
							(p-assign @11.10-11.11 (ident "a"))))))))
	(d-let
		(p-assign @13.1-13.6 (ident "none2"))
		(e-nominal @13.9-13.19 (nominal "Maybe")
			(e-tag @13.9-13.19 (name "None"))))
	(s-nominal-decl @3.1-3.28
		(ty-header @3.1-3.9 (name "maybe")
			(ty-args
				(ty-var @3.7-3.8 (name "a"))))
		(ty-tag-union @3.13-3.28
			(ty-apply @3.14-3.21 (symbol "Some")
				(ty-var @3.19-3.20 (name "a")))
			(ty @3.23-3.27 (name "None")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.6 (type "a -> Maybe(a)"))
		(patt @9.1-9.6 (type "Maybe(a)"))
		(patt @11.1-11.6 (type "_arg -> Maybe(a)"))
		(patt @13.1-13.6 (type "Maybe(a)")))
	(type_decls
		(nominal @3.1-3.28 (type "Maybe(a)")
			(ty-header @3.1-3.9 (name "maybe")
				(ty-args
					(ty-var @3.7-3.8 (name "a"))))))
	(expressions
		(expr @6.9-6.26 (type "a -> Maybe(a)"))
		(expr @9.9-9.19 (type "Maybe(a)"))
		(expr @11.9-11.26 (type "_arg -> Maybe(a)"))
		(expr @13.9-13.19 (type "Maybe(a)"))))
~~~

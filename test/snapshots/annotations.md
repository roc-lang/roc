# META
~~~ini
description=Example of a nominal tag union with a payload
type=file
~~~
# SOURCE
~~~roc
module []

ConsList(a) := [Nil, Cons(a, ConsList(a))]

x : ConsList(Str)
x = ConsList.Cons("hello", ConsList.Nil)
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
UpperIdent(3:1-3:9),NoSpaceOpenRound(3:9-3:10),LowerIdent(3:10-3:11),CloseRound(3:11-3:12),OpColonEqual(3:13-3:15),OpenSquare(3:16-3:17),UpperIdent(3:17-3:20),Comma(3:20-3:21),UpperIdent(3:22-3:26),NoSpaceOpenRound(3:26-3:27),LowerIdent(3:27-3:28),Comma(3:28-3:29),UpperIdent(3:30-3:38),NoSpaceOpenRound(3:38-3:39),LowerIdent(3:39-3:40),CloseRound(3:40-3:41),CloseRound(3:41-3:42),CloseSquare(3:42-3:43),
LowerIdent(5:1-5:2),OpColon(5:3-5:4),UpperIdent(5:5-5:13),NoSpaceOpenRound(5:13-5:14),UpperIdent(5:14-5:17),CloseRound(5:17-5:18),
LowerIdent(6:1-6:2),OpAssign(6:3-6:4),UpperIdent(6:5-6:13),NoSpaceDotUpperIdent(6:13-6:18),NoSpaceOpenRound(6:18-6:19),StringStart(6:19-6:20),StringPart(6:20-6:25),StringEnd(6:25-6:26),Comma(6:26-6:27),UpperIdent(6:28-6:36),NoSpaceDotUpperIdent(6:36-6:40),CloseRound(6:40-6:41),
EndOfFile(7:1-7:1),
~~~
# PARSE
~~~clojure
(file @1.1-6.41
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-type-decl @3.1-3.43
			(header @3.1-3.12 (name "ConsList")
				(args
					(ty-var @3.10-3.11 (raw "a"))))
			(ty-tag-union @3.16-3.43
				(tags
					(ty @3.17-3.20 (name "Nil"))
					(ty-apply @3.22-3.42
						(ty @3.22-3.26 (name "Cons"))
						(ty-var @3.27-3.28 (raw "a"))
						(ty-apply @3.30-3.41
							(ty @3.30-3.38 (name "ConsList"))
							(ty-var @3.39-3.40 (raw "a")))))))
		(s-type-anno @5.1-5.18 (name "x")
			(ty-apply @5.5-5.18
				(ty @5.5-5.13 (name "ConsList"))
				(ty @5.14-5.17 (name "Str"))))
		(s-decl @6.1-6.41
			(p-ident @6.1-6.2 (raw "x"))
			(e-apply @6.5-6.41
				(e-tag @6.5-6.18 (raw "ConsList.Cons"))
				(e-string @6.19-6.26
					(e-string-part @6.20-6.25 (raw "hello")))
				(e-tag @6.28-6.40 (raw "ConsList.Nil"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @6.1-6.2 (ident "x"))
		(e-nominal @6.5-6.41 (nominal "ConsList")
			(e-tag @6.5-6.41 (name "Cons")
				(args
					(e-string @6.19-6.26
						(e-literal @6.20-6.25 (string "hello")))
					(e-nominal @6.28-6.40 (nominal "ConsList")
						(e-tag @6.28-6.40 (name "Nil"))))))
		(annotation @6.1-6.2
			(declared-type
				(ty-apply @5.5-5.18 (name "ConsList") (local)
					(ty-lookup @5.14-5.17 (name "Str") (builtin))))))
	(s-nominal-decl @3.1-3.43
		(ty-header @3.1-3.12 (name "ConsList")
			(ty-args
				(ty-rigid-var @3.10-3.11 (name "a"))))
		(ty-tag-union @3.16-3.43
			(tag_name @3.17-3.20 (name "Nil"))
			(tag_name @3.22-3.42 (name "Cons")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.2 (type "ConsList(Str)")))
	(type_decls
		(nominal @3.1-3.43 (type "Error")
			(ty-header @3.1-3.12 (name "ConsList")
				(ty-args
					(ty-rigid-var @3.10-3.11 (name "a"))))))
	(expressions
		(expr @6.5-6.41 (type "ConsList(Str)"))))
~~~

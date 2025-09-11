# META
~~~ini
description=Example of a nominal tag union with a payload
type=file
~~~
# SOURCE
~~~roc
module []

StrConsList := [Nil, Cons(Str, StrConsList)]

x : StrConsList
x = StrConsList.Cons("hello", StrConsList.Nil)
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
UpperIdent(3:1-3:12),OpColonEqual(3:13-3:15),OpenSquare(3:16-3:17),UpperIdent(3:17-3:20),Comma(3:20-3:21),UpperIdent(3:22-3:26),NoSpaceOpenRound(3:26-3:27),UpperIdent(3:27-3:30),Comma(3:30-3:31),UpperIdent(3:32-3:43),CloseRound(3:43-3:44),CloseSquare(3:44-3:45),
LowerIdent(5:1-5:2),OpColon(5:3-5:4),UpperIdent(5:5-5:16),
LowerIdent(6:1-6:2),OpAssign(6:3-6:4),UpperIdent(6:5-6:16),NoSpaceDotUpperIdent(6:16-6:21),NoSpaceOpenRound(6:21-6:22),StringStart(6:22-6:23),StringPart(6:23-6:28),StringEnd(6:28-6:29),Comma(6:29-6:30),UpperIdent(6:31-6:42),NoSpaceDotUpperIdent(6:42-6:46),CloseRound(6:46-6:47),
EndOfFile(7:1-7:1),
~~~
# PARSE
~~~clojure
(file @1.1-6.47
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-type-decl @3.1-3.45
			(header @3.1-3.12 (name "StrConsList")
				(args))
			(ty-tag-union @3.16-3.45
				(tags
					(ty @3.17-3.20 (name "Nil"))
					(ty-apply @3.22-3.44
						(ty @3.22-3.26 (name "Cons"))
						(ty @3.27-3.30 (name "Str"))
						(ty @3.32-3.43 (name "StrConsList"))))))
		(s-type-anno @5.1-5.16 (name "x")
			(ty @5.5-5.16 (name "StrConsList")))
		(s-decl @6.1-6.47
			(p-ident @6.1-6.2 (raw "x"))
			(e-apply @6.5-6.47
				(e-tag @6.5-6.21 (raw "StrConsList.Cons"))
				(e-string @6.22-6.29
					(e-string-part @6.23-6.28 (raw "hello")))
				(e-tag @6.31-6.46 (raw "StrConsList.Nil"))))))
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
		(e-nominal @6.5-6.47 (nominal "StrConsList")
			(e-tag @6.5-6.47 (name "Cons")
				(args
					(e-string @6.22-6.29
						(e-literal @6.23-6.28 (string "hello")))
					(e-nominal @6.31-6.46 (nominal "StrConsList")
						(e-tag @6.31-6.46 (name "Nil"))))))
		(annotation @6.1-6.2
			(declared-type
				(ty-lookup @5.5-5.16 (name "StrConsList") (local)))))
	(s-nominal-decl @3.1-3.45
		(ty-header @3.1-3.12 (name "StrConsList"))
		(ty-tag-union @3.16-3.45
			(tag_name @3.17-3.20 (name "Nil"))
			(tag_name @3.22-3.44 (name "Cons")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.2 (type "StrConsList")))
	(type_decls
		(nominal @3.1-3.45 (type "StrConsList")
			(ty-header @3.1-3.12 (name "StrConsList"))))
	(expressions
		(expr @6.5-6.47 (type "StrConsList"))))
~~~

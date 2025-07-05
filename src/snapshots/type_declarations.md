# META
~~~ini
description=Various type declarations
type=file
~~~
# SOURCE
~~~roc
module [Map, Foo, Some, Maybe, SomeFunc, add_one, main!]

Map(a, b) : List(a), (a -> b) -> List(b)

Foo : (Bar, Baz)

Some(a) : { foo : Ok(a), bar : Something }

Maybe(a) : [Some(a), None]

SomeFunc(a) : Maybe(a), a -> Maybe(a)

MyType : U64

MyType2 : Module.Thingy
~~~
# EXPECTED
UNDECLARED TYPE - type_declarations.md:5:8:5:11
UNDECLARED TYPE - type_declarations.md:5:13:5:16
UNDECLARED TYPE - type_declarations.md:7:19:7:21
UNDECLARED TYPE - type_declarations.md:7:32:7:41
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:12),Comma(1:12-1:13),UpperIdent(1:14-1:17),Comma(1:17-1:18),UpperIdent(1:19-1:23),Comma(1:23-1:24),UpperIdent(1:25-1:30),Comma(1:30-1:31),UpperIdent(1:32-1:40),Comma(1:40-1:41),LowerIdent(1:42-1:49),Comma(1:49-1:50),LowerIdent(1:51-1:56),CloseSquare(1:56-1:57),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(3:1-3:4),NoSpaceOpenRound(3:4-3:5),LowerIdent(3:5-3:6),Comma(3:6-3:7),LowerIdent(3:8-3:9),CloseRound(3:9-3:10),OpColon(3:11-3:12),UpperIdent(3:13-3:17),NoSpaceOpenRound(3:17-3:18),LowerIdent(3:18-3:19),CloseRound(3:19-3:20),Comma(3:20-3:21),OpenRound(3:22-3:23),LowerIdent(3:23-3:24),OpArrow(3:25-3:27),LowerIdent(3:28-3:29),CloseRound(3:29-3:30),OpArrow(3:31-3:33),UpperIdent(3:34-3:38),NoSpaceOpenRound(3:38-3:39),LowerIdent(3:39-3:40),CloseRound(3:40-3:41),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(5:1-5:4),OpColon(5:5-5:6),OpenRound(5:7-5:8),UpperIdent(5:8-5:11),Comma(5:11-5:12),UpperIdent(5:13-5:16),CloseRound(5:16-5:17),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(7:1-7:5),NoSpaceOpenRound(7:5-7:6),LowerIdent(7:6-7:7),CloseRound(7:7-7:8),OpColon(7:9-7:10),OpenCurly(7:11-7:12),LowerIdent(7:13-7:16),OpColon(7:17-7:18),UpperIdent(7:19-7:21),NoSpaceOpenRound(7:21-7:22),LowerIdent(7:22-7:23),CloseRound(7:23-7:24),Comma(7:24-7:25),LowerIdent(7:26-7:29),OpColon(7:30-7:31),UpperIdent(7:32-7:41),CloseCurly(7:42-7:43),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(9:1-9:6),NoSpaceOpenRound(9:6-9:7),LowerIdent(9:7-9:8),CloseRound(9:8-9:9),OpColon(9:10-9:11),OpenSquare(9:12-9:13),UpperIdent(9:13-9:17),NoSpaceOpenRound(9:17-9:18),LowerIdent(9:18-9:19),CloseRound(9:19-9:20),Comma(9:20-9:21),UpperIdent(9:22-9:26),CloseSquare(9:26-9:27),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(11:1-11:9),NoSpaceOpenRound(11:9-11:10),LowerIdent(11:10-11:11),CloseRound(11:11-11:12),OpColon(11:13-11:14),UpperIdent(11:15-11:20),NoSpaceOpenRound(11:20-11:21),LowerIdent(11:21-11:22),CloseRound(11:22-11:23),Comma(11:23-11:24),LowerIdent(11:25-11:26),OpArrow(11:27-11:29),UpperIdent(11:30-11:35),NoSpaceOpenRound(11:35-11:36),LowerIdent(11:36-11:37),CloseRound(11:37-11:38),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(13:1-13:7),OpColon(13:8-13:9),UpperIdent(13:10-13:13),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(15:1-15:8),OpColon(15:9-15:10),UpperIdent(15:11-15:17),NoSpaceDotUpperIdent(15:17-15:24),EndOfFile(15:24-15:24),
~~~
# PARSE
~~~clojure
(file @1.1-15.24
	(module @1.1-1.57
		(exposes @1.8-1.57
			(exposed-upper-ident (text "Map"))
			(exposed-upper-ident (text "Foo"))
			(exposed-upper-ident (text "Some"))
			(exposed-upper-ident (text "Maybe"))
			(exposed-upper-ident (text "SomeFunc"))
			(exposed-lower-ident (text "add_one"))
			(exposed-lower-ident (text "main!"))))
	(statements
		(s-type-decl @3.1-3.41
			(header @3.1-3.10 (name "Map")
				(args
					(ty-var @3.5-3.6 (raw "a"))
					(ty-var @3.8-3.9 (raw "b"))))
			(ty-fn @3.13-3.41
				(ty-apply @3.13-3.20
					(ty @3.13-3.17 (name "List"))
					(ty-var @3.18-3.19 (raw "a")))
				(ty-fn @3.23-3.29
					(ty-var @3.23-3.24 (raw "a"))
					(ty-var @3.28-3.29 (raw "b")))
				(ty-apply @3.34-3.41
					(ty @3.34-3.38 (name "List"))
					(ty-var @3.39-3.40 (raw "b")))))
		(s-type-decl @5.1-5.17
			(header @5.1-5.4 (name "Foo")
				(args))
			(ty-tuple @5.7-5.17
				(ty @5.8-5.11 (name "Bar"))
				(ty @5.13-5.16 (name "Baz"))))
		(s-type-decl @7.1-7.43
			(header @7.1-7.8 (name "Some")
				(args
					(ty-var @7.6-7.7 (raw "a"))))
			(ty-record @7.11-7.43
				(anno-record-field @7.13-7.25 (name "foo")
					(ty-apply @7.19-7.24
						(ty @7.19-7.21 (name "Ok"))
						(ty-var @7.22-7.23 (raw "a"))))
				(anno-record-field @7.26-7.43 (name "bar")
					(ty @7.32-7.41 (name "Something")))))
		(s-type-decl @9.1-9.27
			(header @9.1-9.9 (name "Maybe")
				(args
					(ty-var @9.7-9.8 (raw "a"))))
			(ty-tag-union @9.12-9.27
				(tags
					(ty-apply @9.13-9.20
						(ty @9.13-9.17 (name "Some"))
						(ty-var @9.18-9.19 (raw "a")))
					(ty @9.22-9.26 (name "None")))))
		(s-type-decl @11.1-11.38
			(header @11.1-11.12 (name "SomeFunc")
				(args
					(ty-var @11.10-11.11 (raw "a"))))
			(ty-fn @11.15-11.38
				(ty-apply @11.15-11.23
					(ty @11.15-11.20 (name "Maybe"))
					(ty-var @11.21-11.22 (raw "a")))
				(ty-var @11.25-11.26 (raw "a"))
				(ty-apply @11.30-11.38
					(ty @11.30-11.35 (name "Maybe"))
					(ty-var @11.36-11.37 (raw "a")))))
		(s-type-decl @13.1-13.13
			(header @13.1-13.7 (name "MyType")
				(args))
			(ty @13.10-13.13 (name "U64")))
		(s-type-decl @15.1-15.24
			(header @15.1-15.8 (name "MyType2")
				(args))
			(ty @15.11-15.24 (name "Module.Thingy")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl @3.1-3.41 (where "TODO")
		(ty-header @3.1-3.10 (name "Map")
			(ty-args
				(ty-var @3.5-3.6 (name "a"))
				(ty-var @3.8-3.9 (name "b"))))
		(ty-fn @3.13-3.41 (effectful false)
			(ty-apply @3.13-3.20 (symbol "List")
				(ty-var @3.18-3.19 (name "a")))
			(ty-parens @3.22-3.30
				(ty-fn @3.23-3.29 (effectful false)
					(ty-var @3.23-3.24 (name "a"))
					(ty-var @3.28-3.29 (name "b"))))
			(ty-apply @3.34-3.41 (symbol "List")
				(ty-var @3.39-3.40 (name "b")))))
	(s-alias-decl @5.1-5.17 (where "TODO")
		(ty-header @5.1-5.4 (name "Foo"))
		(ty-tuple @5.7-5.17
			(ty @5.8-5.11 (name "Bar"))
			(ty @5.13-5.16 (name "Baz"))))
	(s-alias-decl @7.1-7.43 (where "TODO")
		(ty-header @7.1-7.8 (name "Some")
			(ty-args
				(ty-var @7.6-7.7 (name "a"))))
		(ty-record @7.11-7.43
			(field (field "foo")
				(ty-apply @7.19-7.24 (symbol "Ok")
					(ty-var @7.22-7.23 (name "a"))))
			(field (field "bar")
				(ty @7.32-7.41 (name "Something")))))
	(s-alias-decl @9.1-9.27 (where "TODO")
		(ty-header @9.1-9.9 (name "Maybe")
			(ty-args
				(ty-var @9.7-9.8 (name "a"))))
		(ty-tag-union @9.12-9.27
			(ty-apply @9.13-9.20 (symbol "Some")
				(ty-var @9.18-9.19 (name "a")))
			(ty @9.22-9.26 (name "None"))))
	(s-alias-decl @11.1-11.38 (where "TODO")
		(ty-header @11.1-11.12 (name "SomeFunc")
			(ty-args
				(ty-var @11.10-11.11 (name "a"))))
		(ty-fn @11.15-11.38 (effectful false)
			(ty-apply @11.15-11.23 (symbol "Maybe")
				(ty-var @11.21-11.22 (name "a")))
			(ty-var @11.25-11.26 (name "a"))
			(ty-apply @11.30-11.38 (symbol "Maybe")
				(ty-var @11.36-11.37 (name "a")))))
	(s-alias-decl @13.1-13.13 (where "TODO")
		(ty-header @13.1-13.7 (name "MyType"))
		(ty @13.10-13.13 (name "U64")))
	(s-alias-decl @15.1-15.24 (where "TODO")
		(ty-header @15.1-15.8 (name "MyType2"))
		(ty-lookup-external @15.11-15.24
			(ext-decl @15.11-15.24 (ident "Module.Thingy") (kind "type")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~

# META
~~~ini
description=Type alias within associated block referencing another nested type
type=snippet
~~~
# SOURCE
~~~roc
Foo := [Whatever].{
    Bar := [X, Y, Z]

    # Alias within the associated block
    Baz : Foo.Bar

    defaultBaz : Foo.Baz
    defaultBaz = Foo.Bar.X
}

external : Foo.Baz
external = Foo.defaultBaz
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:4),OpColonEqual(1:5-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:17),CloseSquare(1:17-1:18),Dot(1:18-1:19),OpenCurly(1:19-1:20),
UpperIdent(2:5-2:8),OpColonEqual(2:9-2:11),OpenSquare(2:12-2:13),UpperIdent(2:13-2:14),Comma(2:14-2:15),UpperIdent(2:16-2:17),Comma(2:17-2:18),UpperIdent(2:19-2:20),CloseSquare(2:20-2:21),
UpperIdent(5:5-5:8),OpColon(5:9-5:10),UpperIdent(5:11-5:14),NoSpaceDotUpperIdent(5:14-5:18),
LowerIdent(7:5-7:15),OpColon(7:16-7:17),UpperIdent(7:18-7:21),NoSpaceDotUpperIdent(7:21-7:25),
LowerIdent(8:5-8:15),OpAssign(8:16-8:17),UpperIdent(8:18-8:21),NoSpaceDotUpperIdent(8:21-8:25),NoSpaceDotUpperIdent(8:25-8:27),
CloseCurly(9:1-9:2),
LowerIdent(11:1-11:9),OpColon(11:10-11:11),UpperIdent(11:12-11:15),NoSpaceDotUpperIdent(11:15-11:19),
LowerIdent(12:1-12:9),OpAssign(12:10-12:11),UpperIdent(12:12-12:15),NoSpaceDotLowerIdent(12:15-12:26),
EndOfFile(13:1-13:1),
~~~
# PARSE
~~~clojure
(file @1.1-12.26
	(type-module @1.1-1.4)
	(statements
		(s-type-decl @1.1-9.2
			(header @1.1-1.4 (name "Foo")
				(args))
			(ty-tag-union @1.8-1.18
				(tags
					(ty @1.9-1.17 (name "Whatever"))))
			(associated @1.19-9.2
				(s-type-decl @2.5-2.21
					(header @2.5-2.8 (name "Bar")
						(args))
					(ty-tag-union @2.12-2.21
						(tags
							(ty @2.13-2.14 (name "X"))
							(ty @2.16-2.17 (name "Y"))
							(ty @2.19-2.20 (name "Z")))))
				(s-type-decl @5.5-5.18
					(header @5.5-5.8 (name "Baz")
						(args))
					(ty @5.11-5.18 (name "Foo.Bar")))
				(s-type-anno @7.5-7.25 (name "defaultBaz")
					(ty @7.18-7.25 (name "Foo.Baz")))
				(s-decl @8.5-8.27
					(p-ident @8.5-8.15 (raw "defaultBaz"))
					(e-tag @8.18-8.27 (raw "Foo.Bar.X")))))
		(s-type-anno @11.1-11.19 (name "external")
			(ty @11.12-11.19 (name "Foo.Baz")))
		(s-decl @12.1-12.26
			(p-ident @12.1-12.9 (raw "external"))
			(e-ident @12.12-12.26 (raw "Foo.defaultBaz")))))
~~~
# FORMATTED
~~~roc
Foo := [Whatever].{
	Bar := [X, Y, Z]
	Baz : Foo.Bar
	defaultBaz : Foo.Baz
	defaultBaz = Foo.Bar.X
}

external : Foo.Baz
external = Foo.defaultBaz
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @12.1-12.9 (ident "external"))
		(e-lookup-local @12.12-12.26
			(p-assign @8.5-8.27 (ident "Foo.defaultBaz")))
		(annotation @12.1-12.9
			(declared-type
				(ty-lookup @11.12-11.19 (name "Foo.Baz") (local)))))
	(d-let
		(p-assign @8.5-8.27 (ident "Foo.defaultBaz"))
		(e-nominal @8.18-8.27 (nominal "Foo.Bar")
			(e-tag @8.18-8.27 (name "X")))
		(annotation @8.5-8.15
			(declared-type
				(ty-lookup @7.18-7.25 (name "Foo.Baz") (local)))))
	(s-nominal-decl @1.1-9.2
		(ty-header @1.1-1.4 (name "Foo"))
		(ty-tag-union @1.8-1.18
			(ty-tag-name @1.9-1.17 (name "Whatever"))))
	(s-nominal-decl @2.5-2.21
		(ty-header @2.5-2.21 (name "Foo.Bar"))
		(ty-tag-union @2.12-2.21
			(ty-tag-name @2.13-2.14 (name "X"))
			(ty-tag-name @2.16-2.17 (name "Y"))
			(ty-tag-name @2.19-2.20 (name "Z"))))
	(s-alias-decl @5.5-5.18
		(ty-header @5.5-5.18 (name "Foo.Baz"))
		(ty-lookup @5.11-5.18 (name "Foo.Bar") (local))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @12.1-12.9 (type "Foo.Baz"))
		(patt @8.5-8.27 (type "Foo.Baz")))
	(type_decls
		(nominal @1.1-9.2 (type "Foo")
			(ty-header @1.1-1.4 (name "Foo")))
		(nominal @2.5-2.21 (type "Foo.Bar")
			(ty-header @2.5-2.21 (name "Foo.Bar")))
		(alias @5.5-5.18 (type "Foo.Baz")
			(ty-header @5.5-5.18 (name "Foo.Baz"))))
	(expressions
		(expr @12.12-12.26 (type "Foo.Baz"))
		(expr @8.18-8.27 (type "Foo.Baz"))))
~~~

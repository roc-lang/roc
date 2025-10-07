# META
~~~ini
description=Deeply nested tag constructor (three levels deep)
type=snippet
~~~
# SOURCE
~~~roc
Foo := [Whatever].{
    Bar := [Something].{
        Baz := [X, Y, Z]
    }
}

x : Foo.Bar.Baz
x = Foo.Bar.Baz.X
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:4),OpColonEqual(1:5-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:17),CloseSquare(1:17-1:18),Dot(1:18-1:19),OpenCurly(1:19-1:20),
UpperIdent(2:5-2:8),OpColonEqual(2:9-2:11),OpenSquare(2:12-2:13),UpperIdent(2:13-2:22),CloseSquare(2:22-2:23),Dot(2:23-2:24),OpenCurly(2:24-2:25),
UpperIdent(3:9-3:12),OpColonEqual(3:13-3:15),OpenSquare(3:16-3:17),UpperIdent(3:17-3:18),Comma(3:18-3:19),UpperIdent(3:20-3:21),Comma(3:21-3:22),UpperIdent(3:23-3:24),CloseSquare(3:24-3:25),
CloseCurly(4:5-4:6),
CloseCurly(5:1-5:2),
LowerIdent(7:1-7:2),OpColon(7:3-7:4),UpperIdent(7:5-7:8),NoSpaceDotUpperIdent(7:8-7:12),NoSpaceDotUpperIdent(7:12-7:16),
LowerIdent(8:1-8:2),OpAssign(8:3-8:4),UpperIdent(8:5-8:8),NoSpaceDotUpperIdent(8:8-8:12),NoSpaceDotUpperIdent(8:12-8:16),NoSpaceDotUpperIdent(8:16-8:18),
EndOfFile(9:1-9:1),
~~~
# PARSE
~~~clojure
(file @1.1-8.18
	(type-module @1.1-1.4)
	(statements
		(s-type-decl @1.1-5.2
			(header @1.1-1.4 (name "Foo")
				(args))
			(ty-tag-union @1.8-1.18
				(tags
					(ty @1.9-1.17 (name "Whatever"))))
			(associated @1.19-5.2
				(s-type-decl @2.5-4.6
					(header @2.5-2.8 (name "Bar")
						(args))
					(ty-tag-union @2.12-2.23
						(tags
							(ty @2.13-2.22 (name "Something"))))
					(associated @2.24-4.6
						(s-type-decl @3.9-3.25
							(header @3.9-3.12 (name "Baz")
								(args))
							(ty-tag-union @3.16-3.25
								(tags
									(ty @3.17-3.18 (name "X"))
									(ty @3.20-3.21 (name "Y"))
									(ty @3.23-3.24 (name "Z")))))))))
		(s-type-anno @7.1-7.16 (name "x")
			(ty @7.5-7.16 (name "Foo.Bar.Baz")))
		(s-decl @8.1-8.18
			(p-ident @8.1-8.2 (raw "x"))
			(e-tag @8.5-8.18 (raw "Foo.Bar.Baz.X")))))
~~~
# FORMATTED
~~~roc
Foo := [Whatever].{
	Bar := [Something].{
		Baz := [X, Y, Z]
	}
}

x : Foo.Bar.Baz
x = Foo.Bar.Baz.X
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @8.1-8.2 (ident "x"))
		(e-nominal @8.5-8.18 (nominal "Foo.Bar.Baz")
			(e-tag @8.5-8.18 (name "X")))
		(annotation @8.1-8.2
			(declared-type
				(ty-lookup @7.5-7.16 (name "Foo.Bar.Baz") (local)))))
	(s-nominal-decl @1.1-5.2
		(ty-header @1.1-1.4 (name "Foo"))
		(ty-tag-union @1.8-1.18
			(ty-tag-name @1.9-1.17 (name "Whatever"))))
	(s-nominal-decl @2.5-4.6
		(ty-header @2.5-4.6 (name "Foo.Bar"))
		(ty-tag-union @2.12-2.23
			(ty-tag-name @2.13-2.22 (name "Something"))))
	(s-nominal-decl @3.9-3.25
		(ty-header @3.9-3.25 (name "Foo.Bar.Baz"))
		(ty-tag-union @3.16-3.25
			(ty-tag-name @3.17-3.18 (name "X"))
			(ty-tag-name @3.20-3.21 (name "Y"))
			(ty-tag-name @3.23-3.24 (name "Z")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @8.1-8.2 (type "Foo.Bar.Baz")))
	(type_decls
		(nominal @1.1-5.2 (type "Foo")
			(ty-header @1.1-1.4 (name "Foo")))
		(nominal @2.5-4.6 (type "Foo.Bar")
			(ty-header @2.5-4.6 (name "Foo.Bar")))
		(nominal @3.9-3.25 (type "Foo.Bar.Baz")
			(ty-header @3.9-3.25 (name "Foo.Bar.Baz"))))
	(expressions
		(expr @8.5-8.18 (type "Foo.Bar.Baz"))))
~~~

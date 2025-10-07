# META
~~~ini
description=Type annotation referencing nested type directly
type=snippet
~~~
# SOURCE
~~~roc
Foo := [Whatever].{
    Bar := [X, Y, Z]
}

x : Foo.Bar
x = Foo.Bar.X
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:4),OpColonEqual(1:5-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:17),CloseSquare(1:17-1:18),Dot(1:18-1:19),OpenCurly(1:19-1:20),
UpperIdent(2:5-2:8),OpColonEqual(2:9-2:11),OpenSquare(2:12-2:13),UpperIdent(2:13-2:14),Comma(2:14-2:15),UpperIdent(2:16-2:17),Comma(2:17-2:18),UpperIdent(2:19-2:20),CloseSquare(2:20-2:21),
CloseCurly(3:1-3:2),
LowerIdent(5:1-5:2),OpColon(5:3-5:4),UpperIdent(5:5-5:8),NoSpaceDotUpperIdent(5:8-5:12),
LowerIdent(6:1-6:2),OpAssign(6:3-6:4),UpperIdent(6:5-6:8),NoSpaceDotUpperIdent(6:8-6:12),NoSpaceDotUpperIdent(6:12-6:14),
EndOfFile(7:1-7:1),
~~~
# PARSE
~~~clojure
(file @1.1-6.14
	(type-module @1.1-1.4)
	(statements
		(s-type-decl @1.1-3.2
			(header @1.1-1.4 (name "Foo")
				(args))
			(ty-tag-union @1.8-1.18
				(tags
					(ty @1.9-1.17 (name "Whatever"))))
			(associated @1.19-3.2
				(s-type-decl @2.5-2.21
					(header @2.5-2.8 (name "Bar")
						(args))
					(ty-tag-union @2.12-2.21
						(tags
							(ty @2.13-2.14 (name "X"))
							(ty @2.16-2.17 (name "Y"))
							(ty @2.19-2.20 (name "Z")))))))
		(s-type-anno @5.1-5.12 (name "x")
			(ty @5.5-5.12 (name "Foo.Bar")))
		(s-decl @6.1-6.14
			(p-ident @6.1-6.2 (raw "x"))
			(e-tag @6.5-6.14 (raw "Foo.Bar.X")))))
~~~
# FORMATTED
~~~roc
Foo := [Whatever].{
	Bar := [X, Y, Z]
}

x : Foo.Bar
x = Foo.Bar.X
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @6.1-6.2 (ident "x"))
		(e-nominal @6.5-6.14 (nominal "Foo.Bar")
			(e-tag @6.5-6.14 (name "X")))
		(annotation @6.1-6.2
			(declared-type
				(ty-lookup @5.5-5.12 (name "Foo.Bar") (local)))))
	(s-nominal-decl @1.1-3.2
		(ty-header @1.1-1.4 (name "Foo"))
		(ty-tag-union @1.8-1.18
			(ty-tag-name @1.9-1.17 (name "Whatever"))))
	(s-nominal-decl @2.5-2.21
		(ty-header @2.5-2.21 (name "Foo.Bar"))
		(ty-tag-union @2.12-2.21
			(ty-tag-name @2.13-2.14 (name "X"))
			(ty-tag-name @2.16-2.17 (name "Y"))
			(ty-tag-name @2.19-2.20 (name "Z")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.2 (type "Foo.Bar")))
	(type_decls
		(nominal @1.1-3.2 (type "Foo")
			(ty-header @1.1-1.4 (name "Foo")))
		(nominal @2.5-2.21 (type "Foo.Bar")
			(ty-header @2.5-2.21 (name "Foo.Bar"))))
	(expressions
		(expr @6.5-6.14 (type "Foo.Bar"))))
~~~

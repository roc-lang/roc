# META
~~~ini
description=Four-level nested tag constructor (Foo.Bar.Baz.Qux.Tag)
type=snippet
~~~
# SOURCE
~~~roc
Foo := [A].{
    Bar := [B].{
        Baz := [C].{
            Qux := [X, Y, Z]
        }
    }
}

value : Foo.Bar.Baz.Qux
value = Foo.Bar.Baz.Qux.Y
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:4),OpColonEqual(1:5-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:10),CloseSquare(1:10-1:11),Dot(1:11-1:12),OpenCurly(1:12-1:13),
UpperIdent(2:5-2:8),OpColonEqual(2:9-2:11),OpenSquare(2:12-2:13),UpperIdent(2:13-2:14),CloseSquare(2:14-2:15),Dot(2:15-2:16),OpenCurly(2:16-2:17),
UpperIdent(3:9-3:12),OpColonEqual(3:13-3:15),OpenSquare(3:16-3:17),UpperIdent(3:17-3:18),CloseSquare(3:18-3:19),Dot(3:19-3:20),OpenCurly(3:20-3:21),
UpperIdent(4:13-4:16),OpColonEqual(4:17-4:19),OpenSquare(4:20-4:21),UpperIdent(4:21-4:22),Comma(4:22-4:23),UpperIdent(4:24-4:25),Comma(4:25-4:26),UpperIdent(4:27-4:28),CloseSquare(4:28-4:29),
CloseCurly(5:9-5:10),
CloseCurly(6:5-6:6),
CloseCurly(7:1-7:2),
LowerIdent(9:1-9:6),OpColon(9:7-9:8),UpperIdent(9:9-9:12),NoSpaceDotUpperIdent(9:12-9:16),NoSpaceDotUpperIdent(9:16-9:20),NoSpaceDotUpperIdent(9:20-9:24),
LowerIdent(10:1-10:6),OpAssign(10:7-10:8),UpperIdent(10:9-10:12),NoSpaceDotUpperIdent(10:12-10:16),NoSpaceDotUpperIdent(10:16-10:20),NoSpaceDotUpperIdent(10:20-10:24),NoSpaceDotUpperIdent(10:24-10:26),
EndOfFile(11:1-11:1),
~~~
# PARSE
~~~clojure
(file @1.1-10.26
	(type-module @1.1-1.4)
	(statements
		(s-type-decl @1.1-7.2
			(header @1.1-1.4 (name "Foo")
				(args))
			(ty-tag-union @1.8-1.11
				(tags
					(ty @1.9-1.10 (name "A"))))
			(associated @1.12-7.2
				(s-type-decl @2.5-6.6
					(header @2.5-2.8 (name "Bar")
						(args))
					(ty-tag-union @2.12-2.15
						(tags
							(ty @2.13-2.14 (name "B"))))
					(associated @2.16-6.6
						(s-type-decl @3.9-5.10
							(header @3.9-3.12 (name "Baz")
								(args))
							(ty-tag-union @3.16-3.19
								(tags
									(ty @3.17-3.18 (name "C"))))
							(associated @3.20-5.10
								(s-type-decl @4.13-4.29
									(header @4.13-4.16 (name "Qux")
										(args))
									(ty-tag-union @4.20-4.29
										(tags
											(ty @4.21-4.22 (name "X"))
											(ty @4.24-4.25 (name "Y"))
											(ty @4.27-4.28 (name "Z")))))))))))
		(s-type-anno @9.1-9.24 (name "value")
			(ty @9.9-9.24 (name "Foo.Bar.Baz.Qux")))
		(s-decl @10.1-10.26
			(p-ident @10.1-10.6 (raw "value"))
			(e-tag @10.9-10.26 (raw "Foo.Bar.Baz.Qux.Y")))))
~~~
# FORMATTED
~~~roc
Foo := [A].{
	Bar := [B].{
		Baz := [C].{
			Qux := [X, Y, Z]
		}
	}
}

value : Foo.Bar.Baz.Qux
value = Foo.Bar.Baz.Qux.Y
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @10.1-10.6 (ident "value"))
		(e-nominal @10.9-10.26 (nominal "Foo.Bar.Baz.Qux")
			(e-tag @10.9-10.26 (name "Y")))
		(annotation @10.1-10.6
			(declared-type
				(ty-lookup @9.9-9.24 (name "Foo.Bar.Baz.Qux") (local)))))
	(s-nominal-decl @1.1-7.2
		(ty-header @1.1-1.4 (name "Foo"))
		(ty-tag-union @1.8-1.11
			(ty-tag-name @1.9-1.10 (name "A"))))
	(s-nominal-decl @2.5-6.6
		(ty-header @2.5-6.6 (name "Foo.Bar"))
		(ty-tag-union @2.12-2.15
			(ty-tag-name @2.13-2.14 (name "B"))))
	(s-nominal-decl @3.9-5.10
		(ty-header @3.9-5.10 (name "Foo.Bar.Baz"))
		(ty-tag-union @3.16-3.19
			(ty-tag-name @3.17-3.18 (name "C"))))
	(s-nominal-decl @4.13-4.29
		(ty-header @4.13-4.29 (name "Foo.Bar.Baz.Qux"))
		(ty-tag-union @4.20-4.29
			(ty-tag-name @4.21-4.22 (name "X"))
			(ty-tag-name @4.24-4.25 (name "Y"))
			(ty-tag-name @4.27-4.28 (name "Z")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @10.1-10.6 (type "Foo.Bar.Baz.Qux")))
	(type_decls
		(nominal @1.1-7.2 (type "Foo")
			(ty-header @1.1-1.4 (name "Foo")))
		(nominal @2.5-6.6 (type "Foo.Bar")
			(ty-header @2.5-6.6 (name "Foo.Bar")))
		(nominal @3.9-5.10 (type "Foo.Bar.Baz")
			(ty-header @3.9-5.10 (name "Foo.Bar.Baz")))
		(nominal @4.13-4.29 (type "Foo.Bar.Baz.Qux")
			(ty-header @4.13-4.29 (name "Foo.Bar.Baz.Qux"))))
	(expressions
		(expr @10.9-10.26 (type "Foo.Bar.Baz.Qux"))))
~~~

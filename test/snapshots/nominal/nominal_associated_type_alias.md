# META
~~~ini
description=Type alias referencing associated nested type
type=snippet
~~~
# SOURCE
~~~roc
Foo := [Whatever].{
    Bar := [X, Y, Z]
}

# Type alias to the nested type
MyBar : Foo.Bar

useMyBar : MyBar
useMyBar = Foo.Bar.X
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
UpperIdent(6:1-6:6),OpColon(6:7-6:8),UpperIdent(6:9-6:12),NoSpaceDotUpperIdent(6:12-6:16),
LowerIdent(8:1-8:9),OpColon(8:10-8:11),UpperIdent(8:12-8:17),
LowerIdent(9:1-9:9),OpAssign(9:10-9:11),UpperIdent(9:12-9:15),NoSpaceDotUpperIdent(9:15-9:19),NoSpaceDotUpperIdent(9:19-9:21),
EndOfFile(10:1-10:1),
~~~
# PARSE
~~~clojure
(file @1.1-9.21
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
		(s-type-decl @6.1-6.16
			(header @6.1-6.6 (name "MyBar")
				(args))
			(ty @6.9-6.16 (name "Foo.Bar")))
		(s-type-anno @8.1-8.17 (name "useMyBar")
			(ty @8.12-8.17 (name "MyBar")))
		(s-decl @9.1-9.21
			(p-ident @9.1-9.9 (raw "useMyBar"))
			(e-tag @9.12-9.21 (raw "Foo.Bar.X")))))
~~~
# FORMATTED
~~~roc
Foo := [Whatever].{
	Bar := [X, Y, Z]
}

# Type alias to the nested type
MyBar : Foo.Bar

useMyBar : MyBar
useMyBar = Foo.Bar.X
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @9.1-9.9 (ident "useMyBar"))
		(e-nominal @9.12-9.21 (nominal "Foo.Bar")
			(e-tag @9.12-9.21 (name "X")))
		(annotation @9.1-9.9
			(declared-type
				(ty-lookup @8.12-8.17 (name "MyBar") (local)))))
	(s-nominal-decl @1.1-3.2
		(ty-header @1.1-1.4 (name "Foo"))
		(ty-tag-union @1.8-1.18
			(ty-tag-name @1.9-1.17 (name "Whatever"))))
	(s-nominal-decl @2.5-2.21
		(ty-header @2.5-2.21 (name "Foo.Bar"))
		(ty-tag-union @2.12-2.21
			(ty-tag-name @2.13-2.14 (name "X"))
			(ty-tag-name @2.16-2.17 (name "Y"))
			(ty-tag-name @2.19-2.20 (name "Z"))))
	(s-alias-decl @6.1-6.16
		(ty-header @6.1-6.6 (name "MyBar"))
		(ty-lookup @6.9-6.16 (name "Foo.Bar") (local))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @9.1-9.9 (type "MyBar")))
	(type_decls
		(nominal @1.1-3.2 (type "Foo")
			(ty-header @1.1-1.4 (name "Foo")))
		(nominal @2.5-2.21 (type "Foo.Bar")
			(ty-header @2.5-2.21 (name "Foo.Bar")))
		(alias @6.1-6.16 (type "MyBar")
			(ty-header @6.1-6.6 (name "MyBar"))))
	(expressions
		(expr @9.12-9.21 (type "MyBar"))))
~~~

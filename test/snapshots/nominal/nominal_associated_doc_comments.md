# META
~~~ini
description=Doc comments on nested types and associated items
type=file:Foo.roc
~~~
# SOURCE
~~~roc
## Top-level type documentation
Foo := [Whatever].{
    ## Nested type documentation
    Bar := [X, Y, Z]

    ## Associated value documentation
    defaultBar = Foo.Bar.X
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(2:1-2:4),OpColonEqual(2:5-2:7),OpenSquare(2:8-2:9),UpperIdent(2:9-2:17),CloseSquare(2:17-2:18),Dot(2:18-2:19),OpenCurly(2:19-2:20),
UpperIdent(4:5-4:8),OpColonEqual(4:9-4:11),OpenSquare(4:12-4:13),UpperIdent(4:13-4:14),Comma(4:14-4:15),UpperIdent(4:16-4:17),Comma(4:17-4:18),UpperIdent(4:19-4:20),CloseSquare(4:20-4:21),
LowerIdent(7:5-7:15),OpAssign(7:16-7:17),UpperIdent(7:18-7:21),NoSpaceDotUpperIdent(7:21-7:25),NoSpaceDotUpperIdent(7:25-7:27),
CloseCurly(8:1-8:2),
EndOfFile(9:1-9:1),
~~~
# PARSE
~~~clojure
(file @2.1-8.2
	(type-module @2.1-2.4)
	(statements
		(s-type-decl @2.1-8.2
			(header @2.1-2.4 (name "Foo")
				(args))
			(ty-tag-union @2.8-2.18
				(tags
					(ty @2.9-2.17 (name "Whatever"))))
			(associated @2.19-8.2
				(s-type-decl @4.5-4.21
					(header @4.5-4.8 (name "Bar")
						(args))
					(ty-tag-union @4.12-4.21
						(tags
							(ty @4.13-4.14 (name "X"))
							(ty @4.16-4.17 (name "Y"))
							(ty @4.19-4.20 (name "Z")))))
				(s-decl @7.5-7.27
					(p-ident @7.5-7.15 (raw "defaultBar"))
					(e-tag @7.18-7.27 (raw "Foo.Bar.X")))))))
~~~
# FORMATTED
~~~roc
# # Top-level type documentation
Foo := [Whatever].{
	Bar := [X, Y, Z]
	defaultBar = Foo.Bar.X
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @7.5-7.27 (ident "Foo.defaultBar"))
		(e-nominal @7.18-7.27 (nominal "Foo.Bar")
			(e-tag @7.18-7.27 (name "X"))))
	(s-nominal-decl @2.1-8.2
		(ty-header @2.1-2.4 (name "Foo"))
		(ty-tag-union @2.8-2.18
			(ty-tag-name @2.9-2.17 (name "Whatever"))))
	(s-nominal-decl @4.5-4.21
		(ty-header @4.5-4.21 (name "Foo.Bar"))
		(ty-tag-union @4.12-4.21
			(ty-tag-name @4.13-4.14 (name "X"))
			(ty-tag-name @4.16-4.17 (name "Y"))
			(ty-tag-name @4.19-4.20 (name "Z")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @7.5-7.27 (type "Foo.Bar")))
	(type_decls
		(nominal @2.1-8.2 (type "Foo")
			(ty-header @2.1-2.4 (name "Foo")))
		(nominal @4.5-4.21 (type "Foo.Bar")
			(ty-header @4.5-4.21 (name "Foo.Bar"))))
	(expressions
		(expr @7.18-7.27 (type "Foo.Bar"))))
~~~

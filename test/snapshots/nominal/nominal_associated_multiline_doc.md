# META
~~~ini
description=Multi-line doc comments on associated items
type=snippet
~~~
# SOURCE
~~~roc
Foo := [A, B].{
    ## This is a multi-line doc comment
    ## for a nested type declaration
    ## that spans multiple lines
    Bar := [X, Y, Z]

    ## Multi-line documentation
    ## for an associated value
    defaultValue = 42
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:4),OpColonEqual(1:5-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:10),Comma(1:10-1:11),UpperIdent(1:12-1:13),CloseSquare(1:13-1:14),Dot(1:14-1:15),OpenCurly(1:15-1:16),
UpperIdent(5:5-5:8),OpColonEqual(5:9-5:11),OpenSquare(5:12-5:13),UpperIdent(5:13-5:14),Comma(5:14-5:15),UpperIdent(5:16-5:17),Comma(5:17-5:18),UpperIdent(5:19-5:20),CloseSquare(5:20-5:21),
LowerIdent(9:5-9:17),OpAssign(9:18-9:19),Int(9:20-9:22),
CloseCurly(10:1-10:2),
EndOfFile(11:1-11:1),
~~~
# PARSE
~~~clojure
(file @1.1-10.2
	(type-module @1.1-1.4)
	(statements
		(s-type-decl @1.1-10.2
			(header @1.1-1.4 (name "Foo")
				(args))
			(ty-tag-union @1.8-1.14
				(tags
					(ty @1.9-1.10 (name "A"))
					(ty @1.12-1.13 (name "B"))))
			(associated @1.15-10.2
				(s-type-decl @5.5-5.21
					(header @5.5-5.8 (name "Bar")
						(args))
					(ty-tag-union @5.12-5.21
						(tags
							(ty @5.13-5.14 (name "X"))
							(ty @5.16-5.17 (name "Y"))
							(ty @5.19-5.20 (name "Z")))))
				(s-decl @9.5-9.22
					(p-ident @9.5-9.17 (raw "defaultValue"))
					(e-int @9.20-9.22 (raw "42")))))))
~~~
# FORMATTED
~~~roc
Foo := [A, B].{
	Bar := [X, Y, Z]
	defaultValue = 42
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @9.5-9.22 (ident "Foo.defaultValue"))
		(e-num @9.20-9.22 (value "42")))
	(s-nominal-decl @1.1-10.2
		(ty-header @1.1-1.4 (name "Foo"))
		(ty-tag-union @1.8-1.14
			(ty-tag-name @1.9-1.10 (name "A"))
			(ty-tag-name @1.12-1.13 (name "B"))))
	(s-nominal-decl @5.5-5.21
		(ty-header @5.5-5.21 (name "Foo.Bar"))
		(ty-tag-union @5.12-5.21
			(ty-tag-name @5.13-5.14 (name "X"))
			(ty-tag-name @5.16-5.17 (name "Y"))
			(ty-tag-name @5.19-5.20 (name "Z")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @9.5-9.22 (type "Num(_size)")))
	(type_decls
		(nominal @1.1-10.2 (type "Foo")
			(ty-header @1.1-1.4 (name "Foo")))
		(nominal @5.5-5.21 (type "Foo.Bar")
			(ty-header @5.5-5.21 (name "Foo.Bar"))))
	(expressions
		(expr @9.20-9.22 (type "Num(_size)"))))
~~~

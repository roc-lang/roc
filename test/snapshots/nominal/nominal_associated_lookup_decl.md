# META
~~~ini
description=Referencing declaration from associated block
type=file:Foo.roc
~~~
# SOURCE
~~~roc
Foo := [Whatever].{
    bar = 42
}

useBar : U64
useBar = Foo.bar
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:4),OpColonEqual(1:5-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:17),CloseSquare(1:17-1:18),Dot(1:18-1:19),OpenCurly(1:19-1:20),
LowerIdent(2:5-2:8),OpAssign(2:9-2:10),Int(2:11-2:13),
CloseCurly(3:1-3:2),
LowerIdent(5:1-5:7),OpColon(5:8-5:9),UpperIdent(5:10-5:13),
LowerIdent(6:1-6:7),OpAssign(6:8-6:9),UpperIdent(6:10-6:13),NoSpaceDotLowerIdent(6:13-6:17),
EndOfFile(7:1-7:1),
~~~
# PARSE
~~~clojure
(file @1.1-6.17
	(type-module @1.1-1.4)
	(statements
		(s-type-decl @1.1-3.2
			(header @1.1-1.4 (name "Foo")
				(args))
			(ty-tag-union @1.8-1.18
				(tags
					(ty @1.9-1.17 (name "Whatever"))))
			(associated @1.19-3.2
				(s-decl @2.5-2.13
					(p-ident @2.5-2.8 (raw "bar"))
					(e-int @2.11-2.13 (raw "42")))))
		(s-type-anno @5.1-5.13 (name "useBar")
			(ty @5.10-5.13 (name "U64")))
		(s-decl @6.1-6.17
			(p-ident @6.1-6.7 (raw "useBar"))
			(e-ident @6.10-6.17 (raw "Foo.bar")))))
~~~
# FORMATTED
~~~roc
Foo := [Whatever].{
	bar = 42
}

useBar : U64
useBar = Foo.bar
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @6.1-6.7 (ident "useBar"))
		(e-lookup-local @6.10-6.17
			(p-assign @2.5-2.13 (ident "Foo.bar")))
		(annotation @6.1-6.7
			(declared-type
				(ty-lookup @5.10-5.13 (name "U64") (builtin)))))
	(s-nominal-decl @1.1-3.2
		(ty-header @1.1-1.4 (name "Foo"))
		(ty-tag-union @1.8-1.18
			(ty-tag-name @1.9-1.17 (name "Whatever")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.7 (type "Error")))
	(type_decls
		(nominal @1.1-3.2 (type "Foo")
			(ty-header @1.1-1.4 (name "Foo"))))
	(expressions
		(expr @6.10-6.17 (type "Error"))))
~~~

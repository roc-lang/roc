# META
~~~ini
description=Value alias referencing associated item
type=snippet
~~~
# SOURCE
~~~roc
Foo := [Whatever].{
    bar = 42
}

# Value alias to the associated item
myBar : U64
myBar = Foo.bar

result : U64
result = myBar
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
LowerIdent(6:1-6:6),OpColon(6:7-6:8),UpperIdent(6:9-6:12),
LowerIdent(7:1-7:6),OpAssign(7:7-7:8),UpperIdent(7:9-7:12),NoSpaceDotLowerIdent(7:12-7:16),
LowerIdent(9:1-9:7),OpColon(9:8-9:9),UpperIdent(9:10-9:13),
LowerIdent(10:1-10:7),OpAssign(10:8-10:9),LowerIdent(10:10-10:15),
EndOfFile(11:1-11:1),
~~~
# PARSE
~~~clojure
(file @1.1-10.15
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
		(s-type-anno @6.1-6.12 (name "myBar")
			(ty @6.9-6.12 (name "U64")))
		(s-decl @7.1-7.16
			(p-ident @7.1-7.6 (raw "myBar"))
			(e-ident @7.9-7.16 (raw "Foo.bar")))
		(s-type-anno @9.1-9.13 (name "result")
			(ty @9.10-9.13 (name "U64")))
		(s-decl @10.1-10.15
			(p-ident @10.1-10.7 (raw "result"))
			(e-ident @10.10-10.15 (raw "myBar")))))
~~~
# FORMATTED
~~~roc
Foo := [Whatever].{
	bar = 42
}

# Value alias to the associated item
myBar : U64
myBar = Foo.bar

result : U64
result = myBar
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @7.1-7.6 (ident "myBar"))
		(e-lookup-local @7.9-7.16
			(p-assign @2.5-2.13 (ident "Foo.bar")))
		(annotation @7.1-7.6
			(declared-type
				(ty-lookup @6.9-6.12 (name "U64") (builtin)))))
	(d-let
		(p-assign @10.1-10.7 (ident "result"))
		(e-lookup-local @10.10-10.15
			(p-assign @7.1-7.6 (ident "myBar")))
		(annotation @10.1-10.7
			(declared-type
				(ty-lookup @9.10-9.13 (name "U64") (builtin)))))
	(d-let
		(p-assign @2.5-2.13 (ident "Foo.bar"))
		(e-num @2.11-2.13 (value "42")))
	(s-nominal-decl @1.1-3.2
		(ty-header @1.1-1.4 (name "Foo"))
		(ty-tag-union @1.8-1.18
			(ty-tag-name @1.9-1.17 (name "Whatever")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @7.1-7.6 (type "Num(_size)"))
		(patt @10.1-10.7 (type "Error"))
		(patt @2.5-2.13 (type "Num(_size)")))
	(type_decls
		(nominal @1.1-3.2 (type "Foo")
			(ty-header @1.1-1.4 (name "Foo"))))
	(expressions
		(expr @7.9-7.16 (type "Num(_size)"))
		(expr @10.10-10.15 (type "Error"))
		(expr @2.11-2.13 (type "Num(_size)"))))
~~~

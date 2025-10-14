# META
~~~ini
description=Self-references within associated blocks using unqualified names
type=file:Foo.roc
~~~
# SOURCE
~~~roc
Foo := [Whatever].{
    Bar := [X, Y, Z]

    defaultBar : Bar
    defaultBar = X

    transform : Bar -> Bar
    transform = |x| x

    useDefault = transform(defaultBar)
}

external : Foo.Bar
external = Foo.defaultBar
~~~
# EXPECTED
TYPE MISMATCH - nominal_associated_self_reference.md:5:18:5:19
# PROBLEMS
**TYPE MISMATCH**
This expression is used in an unexpected way:
**nominal_associated_self_reference.md:5:18:5:19:**
```roc
    defaultBar = X
```
                 ^

It has the type:
    _[X]_others_

But the type annotation says it should have the type:
    _Foo.Bar_

# TOKENS
~~~zig
UpperIdent(1:1-1:4),OpColonEqual(1:5-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:17),CloseSquare(1:17-1:18),Dot(1:18-1:19),OpenCurly(1:19-1:20),
UpperIdent(2:5-2:8),OpColonEqual(2:9-2:11),OpenSquare(2:12-2:13),UpperIdent(2:13-2:14),Comma(2:14-2:15),UpperIdent(2:16-2:17),Comma(2:17-2:18),UpperIdent(2:19-2:20),CloseSquare(2:20-2:21),
LowerIdent(4:5-4:15),OpColon(4:16-4:17),UpperIdent(4:18-4:21),
LowerIdent(5:5-5:15),OpAssign(5:16-5:17),UpperIdent(5:18-5:19),
LowerIdent(7:5-7:14),OpColon(7:15-7:16),UpperIdent(7:17-7:20),OpArrow(7:21-7:23),UpperIdent(7:24-7:27),
LowerIdent(8:5-8:14),OpAssign(8:15-8:16),OpBar(8:17-8:18),LowerIdent(8:18-8:19),OpBar(8:19-8:20),LowerIdent(8:21-8:22),
LowerIdent(10:5-10:15),OpAssign(10:16-10:17),LowerIdent(10:18-10:27),NoSpaceOpenRound(10:27-10:28),LowerIdent(10:28-10:38),CloseRound(10:38-10:39),
CloseCurly(11:1-11:2),
LowerIdent(13:1-13:9),OpColon(13:10-13:11),UpperIdent(13:12-13:15),NoSpaceDotUpperIdent(13:15-13:19),
LowerIdent(14:1-14:9),OpAssign(14:10-14:11),UpperIdent(14:12-14:15),NoSpaceDotLowerIdent(14:15-14:26),
EndOfFile(15:1-15:1),
~~~
# PARSE
~~~clojure
(file @1.1-14.26
	(type-module @1.1-1.4)
	(statements
		(s-type-decl @1.1-11.2
			(header @1.1-1.4 (name "Foo")
				(args))
			(ty-tag-union @1.8-1.18
				(tags
					(ty @1.9-1.17 (name "Whatever"))))
			(associated @1.19-11.2
				(s-type-decl @2.5-2.21
					(header @2.5-2.8 (name "Bar")
						(args))
					(ty-tag-union @2.12-2.21
						(tags
							(ty @2.13-2.14 (name "X"))
							(ty @2.16-2.17 (name "Y"))
							(ty @2.19-2.20 (name "Z")))))
				(s-type-anno @4.5-4.21 (name "defaultBar")
					(ty @4.18-4.21 (name "Bar")))
				(s-decl @5.5-5.19
					(p-ident @5.5-5.15 (raw "defaultBar"))
					(e-tag @5.18-5.19 (raw "X")))
				(s-type-anno @7.5-7.27 (name "transform")
					(ty-fn @7.17-7.27
						(ty @7.17-7.20 (name "Bar"))
						(ty @7.24-7.27 (name "Bar"))))
				(s-decl @8.5-8.22
					(p-ident @8.5-8.14 (raw "transform"))
					(e-lambda @8.17-8.22
						(args
							(p-ident @8.18-8.19 (raw "x")))
						(e-ident @8.21-8.22 (raw "x"))))
				(s-decl @10.5-10.39
					(p-ident @10.5-10.15 (raw "useDefault"))
					(e-apply @10.18-10.39
						(e-ident @10.18-10.27 (raw "transform"))
						(e-ident @10.28-10.38 (raw "defaultBar"))))))
		(s-type-anno @13.1-13.19 (name "external")
			(ty @13.12-13.19 (name "Foo.Bar")))
		(s-decl @14.1-14.26
			(p-ident @14.1-14.9 (raw "external"))
			(e-ident @14.12-14.26 (raw "Foo.defaultBar")))))
~~~
# FORMATTED
~~~roc
Foo := [Whatever].{
	Bar := [X, Y, Z]
	defaultBar : Bar
	defaultBar = X
	transform : Bar -> Bar
	transform = |x| x
	useDefault = transform(defaultBar)
}

external : Foo.Bar
external = Foo.defaultBar
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @14.1-14.9 (ident "external"))
		(e-lookup-local @14.12-14.26
			(p-assign @5.5-5.19 (ident "Foo.defaultBar")))
		(annotation @14.1-14.9
			(declared-type
				(ty-lookup @13.12-13.19 (name "Foo.Bar") (local)))))
	(d-let
		(p-assign @5.5-5.19 (ident "Foo.defaultBar"))
		(e-tag @5.18-5.19 (name "X"))
		(annotation @5.5-5.15
			(declared-type
				(ty-lookup @4.18-4.21 (name "Bar") (local)))))
	(d-let
		(p-assign @8.5-8.22 (ident "Foo.transform"))
		(e-lambda @8.17-8.22
			(args
				(p-assign @8.18-8.19 (ident "x")))
			(e-lookup-local @8.21-8.22
				(p-assign @8.18-8.19 (ident "x"))))
		(annotation @8.5-8.14
			(declared-type
				(ty-fn @7.17-7.27 (effectful false)
					(ty-lookup @7.17-7.20 (name "Bar") (local))
					(ty-lookup @7.24-7.27 (name "Bar") (local))))))
	(d-let
		(p-assign @10.5-10.39 (ident "Foo.useDefault"))
		(e-call @10.18-10.39
			(e-lookup-local @10.18-10.27
				(p-assign @8.5-8.22 (ident "Foo.transform")))
			(e-lookup-local @10.28-10.38
				(p-assign @5.5-5.19 (ident "Foo.defaultBar")))))
	(s-nominal-decl @1.1-11.2
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
		(patt @14.1-14.9 (type "Error"))
		(patt @5.5-5.19 (type "Error"))
		(patt @8.5-8.22 (type "Foo.Bar -> Foo.Bar"))
		(patt @10.5-10.39 (type "Foo.Bar")))
	(type_decls
		(nominal @1.1-11.2 (type "Foo")
			(ty-header @1.1-1.4 (name "Foo")))
		(nominal @2.5-2.21 (type "Foo.Bar")
			(ty-header @2.5-2.21 (name "Foo.Bar"))))
	(expressions
		(expr @14.12-14.26 (type "Error"))
		(expr @5.18-5.19 (type "Error"))
		(expr @8.17-8.22 (type "Foo.Bar -> Foo.Bar"))
		(expr @10.18-10.39 (type "Foo.Bar"))))
~~~

# META
~~~ini
description=Mixed usage of types and declarations from associated blocks
type=file:Foo.roc
~~~
# SOURCE
~~~roc
Foo := [Whatever].{
    Bar := [A, B, C]

    defaultBar = Bar.A

    transform : Foo.Bar -> Foo.Bar
    transform = |x| x
}

result : Foo.Bar
result = Foo.transform(Foo.defaultBar)
~~~
# EXPECTED
UNDECLARED TYPE - nominal_associated_lookup_mixed.md:4:18:4:21
# PROBLEMS
**UNDECLARED TYPE**
The type _Bar_ is not declared in this scope.

This type is referenced here:
**nominal_associated_lookup_mixed.md:4:18:4:21:**
```roc
    defaultBar = Bar.A
```
                 ^^^


# TOKENS
~~~zig
UpperIdent(1:1-1:4),OpColonEqual(1:5-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:17),CloseSquare(1:17-1:18),Dot(1:18-1:19),OpenCurly(1:19-1:20),
UpperIdent(2:5-2:8),OpColonEqual(2:9-2:11),OpenSquare(2:12-2:13),UpperIdent(2:13-2:14),Comma(2:14-2:15),UpperIdent(2:16-2:17),Comma(2:17-2:18),UpperIdent(2:19-2:20),CloseSquare(2:20-2:21),
LowerIdent(4:5-4:15),OpAssign(4:16-4:17),UpperIdent(4:18-4:21),NoSpaceDotUpperIdent(4:21-4:23),
LowerIdent(6:5-6:14),OpColon(6:15-6:16),UpperIdent(6:17-6:20),NoSpaceDotUpperIdent(6:20-6:24),OpArrow(6:25-6:27),UpperIdent(6:28-6:31),NoSpaceDotUpperIdent(6:31-6:35),
LowerIdent(7:5-7:14),OpAssign(7:15-7:16),OpBar(7:17-7:18),LowerIdent(7:18-7:19),OpBar(7:19-7:20),LowerIdent(7:21-7:22),
CloseCurly(8:1-8:2),
LowerIdent(10:1-10:7),OpColon(10:8-10:9),UpperIdent(10:10-10:13),NoSpaceDotUpperIdent(10:13-10:17),
LowerIdent(11:1-11:7),OpAssign(11:8-11:9),UpperIdent(11:10-11:13),NoSpaceDotLowerIdent(11:13-11:23),NoSpaceOpenRound(11:23-11:24),UpperIdent(11:24-11:27),NoSpaceDotLowerIdent(11:27-11:38),CloseRound(11:38-11:39),
EndOfFile(12:1-12:1),
~~~
# PARSE
~~~clojure
(file @1.1-11.39
	(type-module @1.1-1.4)
	(statements
		(s-type-decl @1.1-8.2
			(header @1.1-1.4 (name "Foo")
				(args))
			(ty-tag-union @1.8-1.18
				(tags
					(ty @1.9-1.17 (name "Whatever"))))
			(associated @1.19-8.2
				(s-type-decl @2.5-2.21
					(header @2.5-2.8 (name "Bar")
						(args))
					(ty-tag-union @2.12-2.21
						(tags
							(ty @2.13-2.14 (name "A"))
							(ty @2.16-2.17 (name "B"))
							(ty @2.19-2.20 (name "C")))))
				(s-decl @4.5-4.23
					(p-ident @4.5-4.15 (raw "defaultBar"))
					(e-tag @4.18-4.23 (raw "Bar.A")))
				(s-type-anno @6.5-6.35 (name "transform")
					(ty-fn @6.17-6.35
						(ty @6.17-6.24 (name "Foo.Bar"))
						(ty @6.28-6.35 (name "Foo.Bar"))))
				(s-decl @7.5-7.22
					(p-ident @7.5-7.14 (raw "transform"))
					(e-lambda @7.17-7.22
						(args
							(p-ident @7.18-7.19 (raw "x")))
						(e-ident @7.21-7.22 (raw "x"))))))
		(s-type-anno @10.1-10.17 (name "result")
			(ty @10.10-10.17 (name "Foo.Bar")))
		(s-decl @11.1-11.39
			(p-ident @11.1-11.7 (raw "result"))
			(e-apply @11.10-11.39
				(e-ident @11.10-11.23 (raw "Foo.transform"))
				(e-ident @11.24-11.38 (raw "Foo.defaultBar"))))))
~~~
# FORMATTED
~~~roc
Foo := [Whatever].{
	Bar := [A, B, C]
	defaultBar = Bar.A
	transform : Foo.Bar -> Foo.Bar
	transform = |x| x
}

result : Foo.Bar
result = Foo.transform(Foo.defaultBar)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @11.1-11.7 (ident "result"))
		(e-call @11.10-11.39
			(e-lookup-local @11.10-11.23
				(p-assign @7.5-7.22 (ident "Foo.transform")))
			(e-lookup-local @11.24-11.38
				(p-assign @4.5-4.23 (ident "Foo.defaultBar"))))
		(annotation @11.1-11.7
			(declared-type
				(ty-lookup @10.10-10.17 (name "Foo.Bar") (local)))))
	(d-let
		(p-assign @4.5-4.23 (ident "Foo.defaultBar"))
		(e-runtime-error (tag "undeclared_type")))
	(d-let
		(p-assign @7.5-7.22 (ident "Foo.transform"))
		(e-lambda @7.17-7.22
			(args
				(p-assign @7.18-7.19 (ident "x")))
			(e-lookup-local @7.21-7.22
				(p-assign @7.18-7.19 (ident "x"))))
		(annotation @7.5-7.14
			(declared-type
				(ty-fn @6.17-6.35 (effectful false)
					(ty-lookup @6.17-6.24 (name "Foo.Bar") (local))
					(ty-lookup @6.28-6.35 (name "Foo.Bar") (local))))))
	(s-nominal-decl @1.1-8.2
		(ty-header @1.1-1.4 (name "Foo"))
		(ty-tag-union @1.8-1.18
			(ty-tag-name @1.9-1.17 (name "Whatever"))))
	(s-nominal-decl @2.5-2.21
		(ty-header @2.5-2.21 (name "Foo.Bar"))
		(ty-tag-union @2.12-2.21
			(ty-tag-name @2.13-2.14 (name "A"))
			(ty-tag-name @2.16-2.17 (name "B"))
			(ty-tag-name @2.19-2.20 (name "C")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @11.1-11.7 (type "Foo.Bar"))
		(patt @4.5-4.23 (type "Error"))
		(patt @7.5-7.22 (type "Foo.Bar -> Foo.Bar")))
	(type_decls
		(nominal @1.1-8.2 (type "Foo")
			(ty-header @1.1-1.4 (name "Foo")))
		(nominal @2.5-2.21 (type "Foo.Bar")
			(ty-header @2.5-2.21 (name "Foo.Bar"))))
	(expressions
		(expr @11.10-11.39 (type "Foo.Bar"))
		(expr @4.18-4.21 (type "Error"))
		(expr @7.17-7.22 (type "Foo.Bar -> Foo.Bar"))))
~~~

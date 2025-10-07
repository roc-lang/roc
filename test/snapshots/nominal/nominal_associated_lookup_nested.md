# META
~~~ini
description=Referencing deeply nested items from associated blocks
type=file:Foo.roc
~~~
# SOURCE
~~~roc
Foo := [Whatever].{
    Bar := [Something].{
        baz = 5
    }
}

myType : Foo.Bar
myType = Something

myNum : U64
myNum = Foo.Bar.baz
~~~
# EXPECTED
TYPE MISMATCH - nominal_associated_lookup_nested.md:8:10:8:19
# PROBLEMS
**TYPE MISMATCH**
This expression is used in an unexpected way:
**nominal_associated_lookup_nested.md:8:10:8:19:**
```roc
myType = Something
```
         ^^^^^^^^^

It has the type:
    _[Something]_others_

But the type annotation says it should have the type:
    _Bar_

# TOKENS
~~~zig
UpperIdent(1:1-1:4),OpColonEqual(1:5-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:17),CloseSquare(1:17-1:18),Dot(1:18-1:19),OpenCurly(1:19-1:20),
UpperIdent(2:5-2:8),OpColonEqual(2:9-2:11),OpenSquare(2:12-2:13),UpperIdent(2:13-2:22),CloseSquare(2:22-2:23),Dot(2:23-2:24),OpenCurly(2:24-2:25),
LowerIdent(3:9-3:12),OpAssign(3:13-3:14),Int(3:15-3:16),
CloseCurly(4:5-4:6),
CloseCurly(5:1-5:2),
LowerIdent(7:1-7:7),OpColon(7:8-7:9),UpperIdent(7:10-7:13),NoSpaceDotUpperIdent(7:13-7:17),
LowerIdent(8:1-8:7),OpAssign(8:8-8:9),UpperIdent(8:10-8:19),
LowerIdent(10:1-10:6),OpColon(10:7-10:8),UpperIdent(10:9-10:12),
LowerIdent(11:1-11:6),OpAssign(11:7-11:8),UpperIdent(11:9-11:12),NoSpaceDotUpperIdent(11:12-11:16),NoSpaceDotLowerIdent(11:16-11:20),
EndOfFile(12:1-12:1),
~~~
# PARSE
~~~clojure
(file @1.1-11.20
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
						(s-decl @3.9-3.16
							(p-ident @3.9-3.12 (raw "baz"))
							(e-int @3.15-3.16 (raw "5")))))))
		(s-type-anno @7.1-7.17 (name "myType")
			(ty @7.10-7.17 (name "Foo.Bar")))
		(s-decl @8.1-8.19
			(p-ident @8.1-8.7 (raw "myType"))
			(e-tag @8.10-8.19 (raw "Something")))
		(s-type-anno @10.1-10.12 (name "myNum")
			(ty @10.9-10.12 (name "U64")))
		(s-decl @11.1-11.20
			(p-ident @11.1-11.6 (raw "myNum"))
			(e-ident @11.9-11.20 (raw "Foo.Bar.baz")))))
~~~
# FORMATTED
~~~roc
Foo := [Whatever].{
	Bar := [Something].{
		baz = 5
	}
}

myType : Foo.Bar
myType = Something

myNum : U64
myNum = Foo.Bar.baz
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @8.1-8.7 (ident "myType"))
		(e-tag @8.10-8.19 (name "Something"))
		(annotation @8.1-8.7
			(declared-type
				(ty-lookup @7.10-7.17 (name "Foo.Bar") (local)))))
	(d-let
		(p-assign @11.1-11.6 (ident "myNum"))
		(e-lookup-local @11.9-11.20
			(p-assign @3.9-3.16 (ident "Foo.Bar.baz")))
		(annotation @11.1-11.6
			(declared-type
				(ty-lookup @10.9-10.12 (name "U64") (builtin)))))
	(s-nominal-decl @1.1-5.2
		(ty-header @1.1-1.4 (name "Foo"))
		(ty-tag-union @1.8-1.18
			(ty-tag-name @1.9-1.17 (name "Whatever"))))
	(s-nominal-decl @2.5-4.6
		(ty-header @2.5-2.8 (name "Bar"))
		(ty-tag-union @2.12-2.23
			(ty-tag-name @2.13-2.22 (name "Something")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @8.1-8.7 (type "Error"))
		(patt @11.1-11.6 (type "Error")))
	(type_decls
		(nominal @1.1-5.2 (type "Foo")
			(ty-header @1.1-1.4 (name "Foo")))
		(nominal @2.5-4.6 (type "Bar")
			(ty-header @2.5-2.8 (name "Bar"))))
	(expressions
		(expr @8.10-8.19 (type "Error"))
		(expr @11.9-11.20 (type "Error"))))
~~~

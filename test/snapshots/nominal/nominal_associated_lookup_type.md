# META
~~~ini
description=Type annotation referencing nested type in associated block
type=file:Foo.roc
~~~
# SOURCE
~~~roc
Foo := [Whatever].{
    Bar := [Something]
}

myBar : Foo.Bar
myBar = Something
~~~
# EXPECTED
TYPE MISMATCH - nominal_associated_lookup_type.md:6:9:6:18
# PROBLEMS
**TYPE MISMATCH**
This expression is used in an unexpected way:
**nominal_associated_lookup_type.md:6:9:6:18:**
```roc
myBar = Something
```
        ^^^^^^^^^

It has the type:
    _[Something]_others_

But the type annotation says it should have the type:
    _Bar_

# TOKENS
~~~zig
UpperIdent(1:1-1:4),OpColonEqual(1:5-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:17),CloseSquare(1:17-1:18),Dot(1:18-1:19),OpenCurly(1:19-1:20),
UpperIdent(2:5-2:8),OpColonEqual(2:9-2:11),OpenSquare(2:12-2:13),UpperIdent(2:13-2:22),CloseSquare(2:22-2:23),
CloseCurly(3:1-3:2),
LowerIdent(5:1-5:6),OpColon(5:7-5:8),UpperIdent(5:9-5:12),NoSpaceDotUpperIdent(5:12-5:16),
LowerIdent(6:1-6:6),OpAssign(6:7-6:8),UpperIdent(6:9-6:18),
EndOfFile(7:1-7:1),
~~~
# PARSE
~~~clojure
(file @1.1-6.18
	(type-module @1.1-1.4)
	(statements
		(s-type-decl @1.1-3.2
			(header @1.1-1.4 (name "Foo")
				(args))
			(ty-tag-union @1.8-1.18
				(tags
					(ty @1.9-1.17 (name "Whatever"))))
			(associated @1.19-3.2
				(s-type-decl @2.5-2.23
					(header @2.5-2.8 (name "Bar")
						(args))
					(ty-tag-union @2.12-2.23
						(tags
							(ty @2.13-2.22 (name "Something")))))))
		(s-type-anno @5.1-5.16 (name "myBar")
			(ty @5.9-5.16 (name "Foo.Bar")))
		(s-decl @6.1-6.18
			(p-ident @6.1-6.6 (raw "myBar"))
			(e-tag @6.9-6.18 (raw "Something")))))
~~~
# FORMATTED
~~~roc
Foo := [Whatever].{
	Bar := [Something]
}

myBar : Foo.Bar
myBar = Something
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @6.1-6.6 (ident "myBar"))
		(e-tag @6.9-6.18 (name "Something"))
		(annotation @6.1-6.6
			(declared-type
				(ty-lookup @5.9-5.16 (name "Foo.Bar") (local)))))
	(s-nominal-decl @1.1-3.2
		(ty-header @1.1-1.4 (name "Foo"))
		(ty-tag-union @1.8-1.18
			(ty-tag-name @1.9-1.17 (name "Whatever"))))
	(s-nominal-decl @2.5-2.23
		(ty-header @2.5-2.8 (name "Bar"))
		(ty-tag-union @2.12-2.23
			(ty-tag-name @2.13-2.22 (name "Something")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.6 (type "Error")))
	(type_decls
		(nominal @1.1-3.2 (type "Foo")
			(ty-header @1.1-1.4 (name "Foo")))
		(nominal @2.5-2.23 (type "Bar")
			(ty-header @2.5-2.8 (name "Bar"))))
	(expressions
		(expr @6.9-6.18 (type "Error"))))
~~~

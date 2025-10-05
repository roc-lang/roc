# META
~~~ini
description=Qualified types in tuples and as type arguments
type=file:Foo.roc
~~~
# SOURCE
~~~roc
Foo := [Whatever].{
    Bar := [X, Y]
    Baz := [Z]
}

pair : (Foo.Bar, Foo.Baz)
pair = (X, Z)

Box : a -> [Box(a)]

boxed : Box(Foo.Bar)
boxed = Box(X)
~~~
# EXPECTED
UNDECLARED TYPE VARIABLE - nominal_associated_in_tuples.md:9:7:9:8
UNDECLARED TYPE VARIABLE - nominal_associated_in_tuples.md:9:17:9:18
TYPE MISMATCH - nominal_associated_in_tuples.md:7:8:7:14
TOO MANY ARGS - nominal_associated_in_tuples.md:11:9:11:21
# PROBLEMS
**UNDECLARED TYPE VARIABLE**
The type variable _a_ is not declared in this scope.

Type variables must be introduced in a type annotation before they can be used.

This type variable is referenced here:
**nominal_associated_in_tuples.md:9:7:9:8:**
```roc
Box : a -> [Box(a)]
```
      ^


**UNDECLARED TYPE VARIABLE**
The type variable _a_ is not declared in this scope.

Type variables must be introduced in a type annotation before they can be used.

This type variable is referenced here:
**nominal_associated_in_tuples.md:9:17:9:18:**
```roc
Box : a -> [Box(a)]
```
                ^


**TYPE MISMATCH**
This expression is used in an unexpected way:
**nominal_associated_in_tuples.md:7:8:7:14:**
```roc
pair = (X, Z)
```
       ^^^^^^

It has the type:
    _([X]_others, [Z]_others2)_

But the type annotation says it should have the type:
    _(Bar, Baz)_

**TOO MANY ARGS**
The type _Box_ expects  argument, but got  instead.
**nominal_associated_in_tuples.md:11:9:11:21:**
```roc
boxed : Box(Foo.Bar)
```
        ^^^^^^^^^^^^



# TOKENS
~~~zig
UpperIdent(1:1-1:4),OpColonEqual(1:5-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:17),CloseSquare(1:17-1:18),Dot(1:18-1:19),OpenCurly(1:19-1:20),
UpperIdent(2:5-2:8),OpColonEqual(2:9-2:11),OpenSquare(2:12-2:13),UpperIdent(2:13-2:14),Comma(2:14-2:15),UpperIdent(2:16-2:17),CloseSquare(2:17-2:18),
UpperIdent(3:5-3:8),OpColonEqual(3:9-3:11),OpenSquare(3:12-3:13),UpperIdent(3:13-3:14),CloseSquare(3:14-3:15),
CloseCurly(4:1-4:2),
LowerIdent(6:1-6:5),OpColon(6:6-6:7),OpenRound(6:8-6:9),UpperIdent(6:9-6:12),NoSpaceDotUpperIdent(6:12-6:16),Comma(6:16-6:17),UpperIdent(6:18-6:21),NoSpaceDotUpperIdent(6:21-6:25),CloseRound(6:25-6:26),
LowerIdent(7:1-7:5),OpAssign(7:6-7:7),OpenRound(7:8-7:9),UpperIdent(7:9-7:10),Comma(7:10-7:11),UpperIdent(7:12-7:13),CloseRound(7:13-7:14),
UpperIdent(9:1-9:4),OpColon(9:5-9:6),LowerIdent(9:7-9:8),OpArrow(9:9-9:11),OpenSquare(9:12-9:13),UpperIdent(9:13-9:16),NoSpaceOpenRound(9:16-9:17),LowerIdent(9:17-9:18),CloseRound(9:18-9:19),CloseSquare(9:19-9:20),
LowerIdent(11:1-11:6),OpColon(11:7-11:8),UpperIdent(11:9-11:12),NoSpaceOpenRound(11:12-11:13),UpperIdent(11:13-11:16),NoSpaceDotUpperIdent(11:16-11:20),CloseRound(11:20-11:21),
LowerIdent(12:1-12:6),OpAssign(12:7-12:8),UpperIdent(12:9-12:12),NoSpaceOpenRound(12:12-12:13),UpperIdent(12:13-12:14),CloseRound(12:14-12:15),
EndOfFile(13:1-13:1),
~~~
# PARSE
~~~clojure
(file @1.1-12.15
	(type-module @1.1-1.4)
	(statements
		(s-type-decl @1.1-4.2
			(header @1.1-1.4 (name "Foo")
				(args))
			(ty-tag-union @1.8-1.18
				(tags
					(ty @1.9-1.17 (name "Whatever"))))
			(associated @1.19-4.2
				(s-type-decl @2.5-2.18
					(header @2.5-2.8 (name "Bar")
						(args))
					(ty-tag-union @2.12-2.18
						(tags
							(ty @2.13-2.14 (name "X"))
							(ty @2.16-2.17 (name "Y")))))
				(s-type-decl @3.5-3.15
					(header @3.5-3.8 (name "Baz")
						(args))
					(ty-tag-union @3.12-3.15
						(tags
							(ty @3.13-3.14 (name "Z")))))))
		(s-type-anno @6.1-6.26 (name "pair")
			(ty-tuple @6.8-6.26
				(ty @6.9-6.16 (name "Foo.Bar"))
				(ty @6.18-6.25 (name "Foo.Baz"))))
		(s-decl @7.1-7.14
			(p-ident @7.1-7.5 (raw "pair"))
			(e-tuple @7.8-7.14
				(e-tag @7.9-7.10 (raw "X"))
				(e-tag @7.12-7.13 (raw "Z"))))
		(s-type-decl @9.1-9.20
			(header @9.1-9.4 (name "Box")
				(args))
			(ty-fn @9.7-9.20
				(ty-var @9.7-9.8 (raw "a"))
				(ty-tag-union @9.12-9.20
					(tags
						(ty-apply @9.13-9.19
							(ty @9.13-9.16 (name "Box"))
							(ty-var @9.17-9.18 (raw "a")))))))
		(s-type-anno @11.1-11.21 (name "boxed")
			(ty-apply @11.9-11.21
				(ty @11.9-11.12 (name "Box"))
				(ty @11.13-11.20 (name "Foo.Bar"))))
		(s-decl @12.1-12.15
			(p-ident @12.1-12.6 (raw "boxed"))
			(e-apply @12.9-12.15
				(e-tag @12.9-12.12 (raw "Box"))
				(e-tag @12.13-12.14 (raw "X"))))))
~~~
# FORMATTED
~~~roc
Foo := [Whatever].{
	Bar := [X, Y]
	Baz := [Z]
}

pair : (Foo.Bar, Foo.Baz)
pair = (X, Z)

Box : a -> [Box(a)]

boxed : Box(Foo.Bar)
boxed = Box(X)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @7.1-7.5 (ident "pair"))
		(e-tuple @7.8-7.14
			(elems
				(e-tag @7.9-7.10 (name "X"))
				(e-tag @7.12-7.13 (name "Z"))))
		(annotation @7.1-7.5
			(declared-type
				(ty-tuple @6.8-6.26
					(ty-lookup @6.9-6.16 (name "Foo.Bar") (local))
					(ty-lookup @6.18-6.25 (name "Foo.Baz") (local))))))
	(d-let
		(p-assign @12.1-12.6 (ident "boxed"))
		(e-tag @12.9-12.15 (name "Box")
			(args
				(e-tag @12.13-12.14 (name "X"))))
		(annotation @12.1-12.6
			(declared-type
				(ty-apply @11.9-11.21 (name "Box") (local)
					(ty-lookup @11.13-11.20 (name "Foo.Bar") (local))))))
	(s-nominal-decl @1.1-4.2
		(ty-header @1.1-1.4 (name "Foo"))
		(ty-tag-union @1.8-1.18
			(ty-tag-name @1.9-1.17 (name "Whatever"))))
	(s-nominal-decl @2.5-2.18
		(ty-header @2.5-2.8 (name "Bar"))
		(ty-tag-union @2.12-2.18
			(ty-tag-name @2.13-2.14 (name "X"))
			(ty-tag-name @2.16-2.17 (name "Y"))))
	(s-nominal-decl @3.5-3.15
		(ty-header @3.5-3.8 (name "Baz"))
		(ty-tag-union @3.12-3.15
			(ty-tag-name @3.13-3.14 (name "Z"))))
	(s-alias-decl @9.1-9.20
		(ty-header @9.1-9.4 (name "Box"))
		(ty-fn @9.7-9.20 (effectful false)
			(ty-malformed @9.7-9.8)
			(ty-tag-union @9.12-9.20
				(ty-tag-name @9.13-9.19 (name "Box")
					(ty-malformed @9.17-9.18))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @7.1-7.5 (type "Error"))
		(patt @12.1-12.6 (type "Error")))
	(type_decls
		(nominal @1.1-4.2 (type "Foo")
			(ty-header @1.1-1.4 (name "Foo")))
		(nominal @2.5-2.18 (type "Bar")
			(ty-header @2.5-2.8 (name "Bar")))
		(nominal @3.5-3.15 (type "Baz")
			(ty-header @3.5-3.8 (name "Baz")))
		(alias @9.1-9.20 (type "Box")
			(ty-header @9.1-9.4 (name "Box"))))
	(expressions
		(expr @7.8-7.14 (type "Error"))
		(expr @12.9-12.15 (type "Error"))))
~~~

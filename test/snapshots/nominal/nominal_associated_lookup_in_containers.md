# META
~~~ini
description=Qualified types in type applications (List, Result, etc.)
type=file:Foo.roc
~~~
# SOURCE
~~~roc
Foo := [Whatever].{
    Bar := [A, B, C]
    Error := [Oops, Yikes]
}

items : List(Foo.Bar)
items = [A, B, C]

result : Result(Foo.Bar, Foo.Error)
result = Ok(A)

nested : { bar : Foo.Bar, count : U64 }
nested = { bar: A, count: 1 }
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDECLARED TYPE**
The type _Result_ is not declared in this scope.

This type is referenced here:
**nominal_associated_lookup_in_containers.md:9:10:9:16:**
```roc
result : Result(Foo.Bar, Foo.Error)
```
         ^^^^^^


# TOKENS
~~~zig
UpperIdent(1:1-1:4),OpColonEqual(1:5-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:17),CloseSquare(1:17-1:18),Dot(1:18-1:19),OpenCurly(1:19-1:20),
UpperIdent(2:5-2:8),OpColonEqual(2:9-2:11),OpenSquare(2:12-2:13),UpperIdent(2:13-2:14),Comma(2:14-2:15),UpperIdent(2:16-2:17),Comma(2:17-2:18),UpperIdent(2:19-2:20),CloseSquare(2:20-2:21),
UpperIdent(3:5-3:10),OpColonEqual(3:11-3:13),OpenSquare(3:14-3:15),UpperIdent(3:15-3:19),Comma(3:19-3:20),UpperIdent(3:21-3:26),CloseSquare(3:26-3:27),
CloseCurly(4:1-4:2),
LowerIdent(6:1-6:6),OpColon(6:7-6:8),UpperIdent(6:9-6:13),NoSpaceOpenRound(6:13-6:14),UpperIdent(6:14-6:17),NoSpaceDotUpperIdent(6:17-6:21),CloseRound(6:21-6:22),
LowerIdent(7:1-7:6),OpAssign(7:7-7:8),OpenSquare(7:9-7:10),UpperIdent(7:10-7:11),Comma(7:11-7:12),UpperIdent(7:13-7:14),Comma(7:14-7:15),UpperIdent(7:16-7:17),CloseSquare(7:17-7:18),
LowerIdent(9:1-9:7),OpColon(9:8-9:9),UpperIdent(9:10-9:16),NoSpaceOpenRound(9:16-9:17),UpperIdent(9:17-9:20),NoSpaceDotUpperIdent(9:20-9:24),Comma(9:24-9:25),UpperIdent(9:26-9:29),NoSpaceDotUpperIdent(9:29-9:35),CloseRound(9:35-9:36),
LowerIdent(10:1-10:7),OpAssign(10:8-10:9),UpperIdent(10:10-10:12),NoSpaceOpenRound(10:12-10:13),UpperIdent(10:13-10:14),CloseRound(10:14-10:15),
LowerIdent(12:1-12:7),OpColon(12:8-12:9),OpenCurly(12:10-12:11),LowerIdent(12:12-12:15),OpColon(12:16-12:17),UpperIdent(12:18-12:21),NoSpaceDotUpperIdent(12:21-12:25),Comma(12:25-12:26),LowerIdent(12:27-12:32),OpColon(12:33-12:34),UpperIdent(12:35-12:38),CloseCurly(12:39-12:40),
LowerIdent(13:1-13:7),OpAssign(13:8-13:9),OpenCurly(13:10-13:11),LowerIdent(13:12-13:15),OpColon(13:15-13:16),UpperIdent(13:17-13:18),Comma(13:18-13:19),LowerIdent(13:20-13:25),OpColon(13:25-13:26),Int(13:27-13:28),CloseCurly(13:29-13:30),
EndOfFile(14:1-14:1),
~~~
# PARSE
~~~clojure
(file @1.1-13.30
	(type-module @1.1-1.4)
	(statements
		(s-type-decl @1.1-4.2
			(header @1.1-1.4 (name "Foo")
				(args))
			(ty-tag-union @1.8-1.18
				(tags
					(ty @1.9-1.17 (name "Whatever"))))
			(associated @1.19-4.2
				(s-type-decl @2.5-2.21
					(header @2.5-2.8 (name "Bar")
						(args))
					(ty-tag-union @2.12-2.21
						(tags
							(ty @2.13-2.14 (name "A"))
							(ty @2.16-2.17 (name "B"))
							(ty @2.19-2.20 (name "C")))))
				(s-type-decl @3.5-3.27
					(header @3.5-3.10 (name "Error")
						(args))
					(ty-tag-union @3.14-3.27
						(tags
							(ty @3.15-3.19 (name "Oops"))
							(ty @3.21-3.26 (name "Yikes")))))))
		(s-type-anno @6.1-6.22 (name "items")
			(ty-apply @6.9-6.22
				(ty @6.9-6.13 (name "List"))
				(ty @6.14-6.21 (name "Foo.Bar"))))
		(s-decl @7.1-7.18
			(p-ident @7.1-7.6 (raw "items"))
			(e-list @7.9-7.18
				(e-tag @7.10-7.11 (raw "A"))
				(e-tag @7.13-7.14 (raw "B"))
				(e-tag @7.16-7.17 (raw "C"))))
		(s-type-anno @9.1-9.36 (name "result")
			(ty-apply @9.10-9.36
				(ty @9.10-9.16 (name "Result"))
				(ty @9.17-9.24 (name "Foo.Bar"))
				(ty @9.26-9.35 (name "Foo.Error"))))
		(s-decl @10.1-10.15
			(p-ident @10.1-10.7 (raw "result"))
			(e-apply @10.10-10.15
				(e-tag @10.10-10.12 (raw "Ok"))
				(e-tag @10.13-10.14 (raw "A"))))
		(s-type-anno @12.1-12.40 (name "nested")
			(ty-record @12.10-12.40
				(anno-record-field @12.12-12.25 (name "bar")
					(ty @12.18-12.25 (name "Foo.Bar")))
				(anno-record-field @12.27-12.38 (name "count")
					(ty @12.35-12.38 (name "U64")))))
		(s-decl @13.1-13.30
			(p-ident @13.1-13.7 (raw "nested"))
			(e-record @13.10-13.30
				(field (field "bar")
					(e-tag @13.17-13.18 (raw "A")))
				(field (field "count")
					(e-int @13.27-13.28 (raw "1")))))))
~~~
# FORMATTED
~~~roc
Foo := [Whatever].{
	Bar := [A, B, C]
	Error := [Oops, Yikes]
}

items : List(Foo.Bar)
items = [A, B, C]

result : Result(Foo.Bar, Foo.Error)
result = Ok(A)

nested : { bar : Foo.Bar, count : U64 }
nested = { bar: A, count: 1 }
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @7.1-7.6 (ident "items"))
		(e-list @7.9-7.18
			(elems
				(e-tag @7.10-7.11 (name "A"))
				(e-tag @7.13-7.14 (name "B"))
				(e-tag @7.16-7.17 (name "C"))))
		(annotation @7.1-7.6
			(declared-type
				(ty-apply @6.9-6.22 (name "List") (builtin)
					(ty-lookup @6.14-6.21 (name "Foo.Bar") (local))))))
	(d-let
		(p-assign @10.1-10.7 (ident "result"))
		(e-tag @10.10-10.15 (name "Ok")
			(args
				(e-tag @10.13-10.14 (name "A"))))
		(annotation @10.1-10.7
			(declared-type
				(ty-malformed @9.10-9.16))))
	(d-let
		(p-assign @13.1-13.7 (ident "nested"))
		(e-record @13.10-13.30
			(fields
				(field (name "bar")
					(e-tag @13.17-13.18 (name "A")))
				(field (name "count")
					(e-num @13.27-13.28 (value "1")))))
		(annotation @13.1-13.7
			(declared-type
				(ty-record @12.10-12.40
					(field (field "bar")
						(ty-lookup @12.18-12.25 (name "Foo.Bar") (local)))
					(field (field "count")
						(ty-lookup @12.35-12.38 (name "U64") (builtin)))))))
	(s-nominal-decl @1.1-4.2
		(ty-header @1.1-1.4 (name "Foo"))
		(ty-tag-union @1.8-1.18
			(ty-tag-name @1.9-1.17 (name "Whatever"))))
	(s-nominal-decl @2.5-2.21
		(ty-header @2.5-2.21 (name "Foo.Bar"))
		(ty-tag-union @2.12-2.21
			(ty-tag-name @2.13-2.14 (name "A"))
			(ty-tag-name @2.16-2.17 (name "B"))
			(ty-tag-name @2.19-2.20 (name "C"))))
	(s-nominal-decl @3.5-3.27
		(ty-header @3.5-3.27 (name "Foo.Error"))
		(ty-tag-union @3.14-3.27
			(ty-tag-name @3.15-3.19 (name "Oops"))
			(ty-tag-name @3.21-3.26 (name "Yikes")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @7.1-7.6 (type "List(Foo.Bar)"))
		(patt @10.1-10.7 (type "Error"))
		(patt @13.1-13.7 (type "{ bar: Foo.Bar, count: Num(Int(Unsigned64)) }")))
	(type_decls
		(nominal @1.1-4.2 (type "Foo")
			(ty-header @1.1-1.4 (name "Foo")))
		(nominal @2.5-2.21 (type "Foo.Bar")
			(ty-header @2.5-2.21 (name "Foo.Bar")))
		(nominal @3.5-3.27 (type "Foo.Error")
			(ty-header @3.5-3.27 (name "Foo.Error"))))
	(expressions
		(expr @7.9-7.18 (type "List(Foo.Bar)"))
		(expr @10.10-10.15 (type "Error"))
		(expr @13.10-13.30 (type "{ bar: Foo.Bar, count: Num(Int(Unsigned64)) }"))))
~~~

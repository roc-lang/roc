# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module []

foo : U64
bar : Thing(a, b, _)
biz : (a, b, c)
add_one : (
U8, U16 -> U32)
main! : List(String) -> Result({}, _)
tag_tuple : Value((a, b, c))
~~~
# EXPECTED
ASCII CONTROL CHARACTER - :0:0:0:0
UNDECLARED TYPE - fuzz_crash_048.md:4:7:4:12
UNDECLARED TYPE - fuzz_crash_048.md:8:14:8:20
UNDECLARED TYPE - fuzz_crash_048.md:9:13:9:18
# PROBLEMS
**ASCII CONTROL CHARACTER**
ASCII control characters are not allowed in Roc source code.



**UNDECLARED TYPE**
The type _Thing_ is not declared in this scope.

This type is referenced here:
**fuzz_crash_048.md:4:7:4:12:**
```roc
bar : Thing(a, b, _)
```
      ^^^^^


**UNDECLARED TYPE**
The type _String_ is not declared in this scope.

This type is referenced here:
**fuzz_crash_048.md:8:14:8:20:**
```roc
main! : List(String) -> Result({}, _)
```
             ^^^^^^


**UNDECLARED TYPE**
The type _Value_ is not declared in this scope.

This type is referenced here:
**fuzz_crash_048.md:9:13:9:18:**
```roc
tag_tuple : Value((a, b, c))
```
            ^^^^^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
LowerIdent(3:1-3:4),OpColon(3:5-3:6),UpperIdent(3:7-3:10),
LowerIdent(4:1-4:4),OpColon(4:5-4:6),UpperIdent(4:7-4:12),NoSpaceOpenRound(4:12-4:13),LowerIdent(4:13-4:14),Comma(4:14-4:15),LowerIdent(4:16-4:17),Comma(4:17-4:18),Underscore(4:19-4:20),CloseRound(4:20-4:21),
LowerIdent(5:1-5:4),OpColon(5:5-5:6),OpenRound(5:7-5:8),LowerIdent(5:8-5:9),Comma(5:9-5:10),LowerIdent(5:11-5:12),Comma(5:12-5:13),LowerIdent(5:14-5:15),CloseRound(5:15-5:16),
LowerIdent(6:1-6:8),OpColon(6:9-6:10),OpenRound(6:11-6:12),
UpperIdent(7:1-7:3),Comma(7:3-7:4),UpperIdent(7:5-7:8),OpArrow(7:9-7:11),UpperIdent(7:12-7:15),CloseRound(7:15-7:16),
LowerIdent(8:1-8:6),OpColon(8:7-8:8),UpperIdent(8:9-8:13),NoSpaceOpenRound(8:13-8:14),UpperIdent(8:14-8:20),CloseRound(8:20-8:21),OpArrow(8:22-8:24),UpperIdent(8:25-8:31),NoSpaceOpenRound(8:31-8:32),OpenCurly(8:32-8:33),CloseCurly(8:33-8:34),Comma(8:34-8:35),Underscore(8:36-8:37),CloseRound(8:37-8:38),
LowerIdent(9:1-9:10),OpColon(9:11-9:12),UpperIdent(9:13-9:18),NoSpaceOpenRound(9:18-9:19),NoSpaceOpenRound(9:19-9:20),LowerIdent(9:20-9:21),Comma(9:21-9:22),LowerIdent(9:23-9:24),Comma(9:24-9:25),LowerIdent(9:26-9:27),CloseRound(9:27-9:28),CloseRound(9:28-9:29),
EndOfFile(10:1-10:1),
~~~
# PARSE
~~~clojure
(file @1.1-9.29
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-type-anno @3.1-3.10 (name "foo")
			(ty @3.7-3.10 (name "U64")))
		(s-type-anno @4.1-4.21 (name "bar")
			(ty-apply @4.7-4.21
				(ty @4.7-4.12 (name "Thing"))
				(ty-var @4.13-4.14 (raw "a"))
				(ty-var @4.16-4.17 (raw "b"))
				(_)))
		(s-type-anno @5.1-5.16 (name "biz")
			(ty-tuple @5.7-5.16
				(ty-var @5.8-5.9 (raw "a"))
				(ty-var @5.11-5.12 (raw "b"))
				(ty-var @5.14-5.15 (raw "c"))))
		(s-type-anno @6.1-7.16 (name "add_one")
			(ty-fn @7.1-7.15
				(ty @7.1-7.3 (name "U8"))
				(ty @7.5-7.8 (name "U16"))
				(ty @7.12-7.15 (name "U32"))))
		(s-type-anno @8.1-8.38 (name "main!")
			(ty-fn @8.9-8.38
				(ty-apply @8.9-8.21
					(ty @8.9-8.13 (name "List"))
					(ty @8.14-8.20 (name "String")))
				(ty-apply @8.25-8.38
					(ty @8.25-8.31 (name "Result"))
					(ty-record @8.32-8.34)
					(_))))
		(s-type-anno @9.1-9.29 (name "tag_tuple")
			(ty-apply @9.13-9.29
				(ty @9.13-9.18 (name "Value"))
				(ty-tuple @9.19-9.28
					(ty-var @9.20-9.21 (raw "a"))
					(ty-var @9.23-9.24 (raw "b"))
					(ty-var @9.26-9.27 (raw "c")))))))
~~~
# FORMATTED
~~~roc
module []

foo : U64
bar : Thing(a, b, _)
biz : (a, b, c)
add_one : (
	U8, U16 -> U32)
main! : List(String) -> Result({}, _)
tag_tuple : Value((a, b, c))
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-nominal-decl @1.1-1.1
		(ty-header @1.1-1.1 (name "Bool"))
		(ty-tag-union @1.1-1.1
			(tag_name @1.1-1.1 (name "True"))
			(tag_name @1.1-1.1 (name "False"))))
	(s-nominal-decl @1.1-1.1
		(ty-header @1.1-1.1 (name "Result")
			(ty-args
				(ty-rigid-var @1.1-1.1 (name "ok"))
				(ty-rigid-var @1.1-1.1 (name "err"))))
		(ty-tag-union @1.1-1.1
			(tag_name @1.1-1.1 (name "Ok"))
			(tag_name @1.1-1.1 (name "Err")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(nominal @1.1-1.1 (type "Bool")
			(ty-header @1.1-1.1 (name "Bool")))
		(nominal @1.1-1.1 (type "Result(ok, err)")
			(ty-header @1.1-1.1 (name "Result")
				(ty-args
					(ty-rigid-var @1.1-1.1 (name "ok"))
					(ty-rigid-var @1.1-1.1 (name "err"))))))
	(expressions))
~~~

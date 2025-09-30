# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
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
MISSING MAIN! FUNCTION - fuzz_crash_048.md:1:1:7:29
UNDECLARED TYPE - fuzz_crash_048.md:2:7:2:12
UNDECLARED TYPE - fuzz_crash_048.md:6:14:6:20
UNDECLARED TYPE - fuzz_crash_048.md:7:13:7:18
# PROBLEMS
**ASCII CONTROL CHARACTER**
ASCII control characters are not allowed in Roc source code.



**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**fuzz_crash_048.md:1:1:7:29:**
```roc
foo : U64
bar : Thing(a, b, _)
biz : (a, b, c)
add_one : (
U8, U16 -> U32)
main! : List(String) -> Result({}, _)
tag_tuple : Value((a, b, c))
```


**UNDECLARED TYPE**
The type _Thing_ is not declared in this scope.

This type is referenced here:
**fuzz_crash_048.md:2:7:2:12:**
```roc
bar : Thing(a, b, _)
```
      ^^^^^


**UNDECLARED TYPE**
The type _String_ is not declared in this scope.

This type is referenced here:
**fuzz_crash_048.md:6:14:6:20:**
```roc
main! : List(String) -> Result({}, _)
```
             ^^^^^^


**UNDECLARED TYPE**
The type _Value_ is not declared in this scope.

This type is referenced here:
**fuzz_crash_048.md:7:13:7:18:**
```roc
tag_tuple : Value((a, b, c))
```
            ^^^^^


# TOKENS
~~~zig
LowerIdent(1:1-1:4),OpColon(1:5-1:6),UpperIdent(1:7-1:10),
LowerIdent(2:1-2:4),OpColon(2:5-2:6),UpperIdent(2:7-2:12),NoSpaceOpenRound(2:12-2:13),LowerIdent(2:13-2:14),Comma(2:14-2:15),LowerIdent(2:16-2:17),Comma(2:17-2:18),Underscore(2:19-2:20),CloseRound(2:20-2:21),
LowerIdent(3:1-3:4),OpColon(3:5-3:6),OpenRound(3:7-3:8),LowerIdent(3:8-3:9),Comma(3:9-3:10),LowerIdent(3:11-3:12),Comma(3:12-3:13),LowerIdent(3:14-3:15),CloseRound(3:15-3:16),
LowerIdent(4:1-4:8),OpColon(4:9-4:10),OpenRound(4:11-4:12),
UpperIdent(5:1-5:3),Comma(5:3-5:4),UpperIdent(5:5-5:8),OpArrow(5:9-5:11),UpperIdent(5:12-5:15),CloseRound(5:15-5:16),
LowerIdent(6:1-6:6),OpColon(6:7-6:8),UpperIdent(6:9-6:13),NoSpaceOpenRound(6:13-6:14),UpperIdent(6:14-6:20),CloseRound(6:20-6:21),OpArrow(6:22-6:24),UpperIdent(6:25-6:31),NoSpaceOpenRound(6:31-6:32),OpenCurly(6:32-6:33),CloseCurly(6:33-6:34),Comma(6:34-6:35),Underscore(6:36-6:37),CloseRound(6:37-6:38),
LowerIdent(7:1-7:10),OpColon(7:11-7:12),UpperIdent(7:13-7:18),NoSpaceOpenRound(7:18-7:19),NoSpaceOpenRound(7:19-7:20),LowerIdent(7:20-7:21),Comma(7:21-7:22),LowerIdent(7:23-7:24),Comma(7:24-7:25),LowerIdent(7:26-7:27),CloseRound(7:27-7:28),CloseRound(7:28-7:29),
EndOfFile(8:1-8:1),
~~~
# PARSE
~~~clojure
(file @1.1-7.29
	(type-module @1.1-1.4)
	(statements
		(s-type-anno @1.1-1.10 (name "foo")
			(ty @1.7-1.10 (name "U64")))
		(s-type-anno @2.1-2.21 (name "bar")
			(ty-apply @2.7-2.21
				(ty @2.7-2.12 (name "Thing"))
				(ty-var @2.13-2.14 (raw "a"))
				(ty-var @2.16-2.17 (raw "b"))
				(_)))
		(s-type-anno @3.1-3.16 (name "biz")
			(ty-tuple @3.7-3.16
				(ty-var @3.8-3.9 (raw "a"))
				(ty-var @3.11-3.12 (raw "b"))
				(ty-var @3.14-3.15 (raw "c"))))
		(s-type-anno @4.1-5.16 (name "add_one")
			(ty-fn @5.1-5.15
				(ty @5.1-5.3 (name "U8"))
				(ty @5.5-5.8 (name "U16"))
				(ty @5.12-5.15 (name "U32"))))
		(s-type-anno @6.1-6.38 (name "main!")
			(ty-fn @6.9-6.38
				(ty-apply @6.9-6.21
					(ty @6.9-6.13 (name "List"))
					(ty @6.14-6.20 (name "String")))
				(ty-apply @6.25-6.38
					(ty @6.25-6.31 (name "Result"))
					(ty-record @6.32-6.34)
					(_))))
		(s-type-anno @7.1-7.29 (name "tag_tuple")
			(ty-apply @7.13-7.29
				(ty @7.13-7.18 (name "Value"))
				(ty-tuple @7.19-7.28
					(ty-var @7.20-7.21 (raw "a"))
					(ty-var @7.23-7.24 (raw "b"))
					(ty-var @7.26-7.27 (raw "c")))))))
~~~
# FORMATTED
~~~roc
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
(can-ir (empty true))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~

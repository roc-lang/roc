# META
~~~ini
description=Various type annotations
type=file
~~~
# SOURCE
~~~roc
module []

foo : U64
bar : Thing(_a, _b, _)
baz : (_a, _b, _c)
add_one : (U8, U16 -> U32)
main! : List(String) -> Result({}, _)
tag_tuple : Value((_a, _b, _c))
~~~
# EXPECTED
COMPILER DIAGNOSTIC - type_annotations.md:0:0:0:0
COMPILER DIAGNOSTIC - type_annotations.md:0:0:0:0
COMPILER DIAGNOSTIC - type_annotations.md:0:0:0:0
# PROBLEMS
**UNDECLARED TYPE**
The type _Thing_ is not declared in this scope.

This type is referenced here:
**type_annotations.md:4:7:4:12:**
```roc
bar : Thing(_a, _b, _)
```
      ^^^^^


**UNDECLARED TYPE**
The type _String_ is not declared in this scope.

This type is referenced here:
**type_annotations.md:7:14:7:20:**
```roc
main! : List(String) -> Result({}, _)
```
             ^^^^^^


**UNDECLARED TYPE**
The type _Value_ is not declared in this scope.

This type is referenced here:
**type_annotations.md:8:13:8:18:**
```roc
tag_tuple : Value((_a, _b, _c))
```
            ^^^^^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
LowerIdent(3:1-3:4),OpColon(3:5-3:6),UpperIdent(3:7-3:10),
LowerIdent(4:1-4:4),OpColon(4:5-4:6),UpperIdent(4:7-4:12),NoSpaceOpenRound(4:12-4:13),NamedUnderscore(4:13-4:15),Comma(4:15-4:16),NamedUnderscore(4:17-4:19),Comma(4:19-4:20),Underscore(4:21-4:22),CloseRound(4:22-4:23),
LowerIdent(5:1-5:4),OpColon(5:5-5:6),OpenRound(5:7-5:8),NamedUnderscore(5:8-5:10),Comma(5:10-5:11),NamedUnderscore(5:12-5:14),Comma(5:14-5:15),NamedUnderscore(5:16-5:18),CloseRound(5:18-5:19),
LowerIdent(6:1-6:8),OpColon(6:9-6:10),OpenRound(6:11-6:12),UpperIdent(6:12-6:14),Comma(6:14-6:15),UpperIdent(6:16-6:19),OpArrow(6:20-6:22),UpperIdent(6:23-6:26),CloseRound(6:26-6:27),
LowerIdent(7:1-7:6),OpColon(7:7-7:8),UpperIdent(7:9-7:13),NoSpaceOpenRound(7:13-7:14),UpperIdent(7:14-7:20),CloseRound(7:20-7:21),OpArrow(7:22-7:24),UpperIdent(7:25-7:31),NoSpaceOpenRound(7:31-7:32),OpenCurly(7:32-7:33),CloseCurly(7:33-7:34),Comma(7:34-7:35),Underscore(7:36-7:37),CloseRound(7:37-7:38),
LowerIdent(8:1-8:10),OpColon(8:11-8:12),UpperIdent(8:13-8:18),NoSpaceOpenRound(8:18-8:19),NoSpaceOpenRound(8:19-8:20),NamedUnderscore(8:20-8:22),Comma(8:22-8:23),NamedUnderscore(8:24-8:26),Comma(8:26-8:27),NamedUnderscore(8:28-8:30),CloseRound(8:30-8:31),CloseRound(8:31-8:32),EndOfFile(8:32-8:32),
~~~
# PARSE
~~~clojure
(file @1.1-8.32
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-type-anno @3.1-3.10 (name "foo")
			(ty @3.7-3.10 (name "U64")))
		(s-type-anno @4.1-4.23 (name "bar")
			(ty-apply @4.7-4.23
				(ty @4.7-4.12 (name "Thing"))
				(underscore-ty-var @4.13-4.15 (raw "_a"))
				(underscore-ty-var @4.17-4.19 (raw "_b"))
				(_)))
		(s-type-anno @5.1-5.19 (name "baz")
			(ty-tuple @5.7-5.19
				(underscore-ty-var @5.8-5.10 (raw "_a"))
				(underscore-ty-var @5.12-5.14 (raw "_b"))
				(underscore-ty-var @5.16-5.18 (raw "_c"))))
		(s-type-anno @6.1-6.27 (name "add_one")
			(ty-fn @6.12-6.26
				(ty @6.12-6.14 (name "U8"))
				(ty @6.16-6.19 (name "U16"))
				(ty @6.23-6.26 (name "U32"))))
		(s-type-anno @7.1-7.38 (name "main!")
			(ty-fn @7.9-7.38
				(ty-apply @7.9-7.21
					(ty @7.9-7.13 (name "List"))
					(ty @7.14-7.20 (name "String")))
				(ty-apply @7.25-7.38
					(ty @7.25-7.31 (name "Result"))
					(ty-record @7.32-7.34)
					(_))))
		(s-type-anno @8.1-8.32 (name "tag_tuple")
			(ty-apply @8.13-8.32
				(ty @8.13-8.18 (name "Value"))
				(ty-tuple @8.19-8.31
					(underscore-ty-var @8.20-8.22 (raw "_a"))
					(underscore-ty-var @8.24-8.26 (raw "_b"))
					(underscore-ty-var @8.28-8.30 (raw "_c")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
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

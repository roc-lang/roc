# META
~~~ini
description=Various type annotations
type=snippet
~~~
# SOURCE
~~~roc
foo : U64
bar : Thing(_a, _b, _)
baz : (_a, _b, _c)
add_one : (U8, U16 -> U32)
main! : List(String) -> Result({}, _)
tag_tuple : Value((_a, _b, _c))
~~~
# EXPECTED
UNDECLARED TYPE - type_annotations.md:2:7:2:12
UNDECLARED TYPE - type_annotations.md:5:14:5:20
UNDECLARED TYPE - type_annotations.md:6:13:6:18
# PROBLEMS
**UNDECLARED TYPE**
The type _Thing_ is not declared in this scope.

This type is referenced here:
**type_annotations.md:2:7:2:12:**
```roc
bar : Thing(_a, _b, _)
```
      ^^^^^


**UNDECLARED TYPE**
The type _String_ is not declared in this scope.

This type is referenced here:
**type_annotations.md:5:14:5:20:**
```roc
main! : List(String) -> Result({}, _)
```
             ^^^^^^


**UNDECLARED TYPE**
The type _Value_ is not declared in this scope.

This type is referenced here:
**type_annotations.md:6:13:6:18:**
```roc
tag_tuple : Value((_a, _b, _c))
```
            ^^^^^


# TOKENS
~~~zig
LowerIdent(1:1-1:4),OpColon(1:5-1:6),UpperIdent(1:7-1:10),
LowerIdent(2:1-2:4),OpColon(2:5-2:6),UpperIdent(2:7-2:12),NoSpaceOpenRound(2:12-2:13),NamedUnderscore(2:13-2:15),Comma(2:15-2:16),NamedUnderscore(2:17-2:19),Comma(2:19-2:20),Underscore(2:21-2:22),CloseRound(2:22-2:23),
LowerIdent(3:1-3:4),OpColon(3:5-3:6),OpenRound(3:7-3:8),NamedUnderscore(3:8-3:10),Comma(3:10-3:11),NamedUnderscore(3:12-3:14),Comma(3:14-3:15),NamedUnderscore(3:16-3:18),CloseRound(3:18-3:19),
LowerIdent(4:1-4:8),OpColon(4:9-4:10),OpenRound(4:11-4:12),UpperIdent(4:12-4:14),Comma(4:14-4:15),UpperIdent(4:16-4:19),OpArrow(4:20-4:22),UpperIdent(4:23-4:26),CloseRound(4:26-4:27),
LowerIdent(5:1-5:6),OpColon(5:7-5:8),UpperIdent(5:9-5:13),NoSpaceOpenRound(5:13-5:14),UpperIdent(5:14-5:20),CloseRound(5:20-5:21),OpArrow(5:22-5:24),UpperIdent(5:25-5:31),NoSpaceOpenRound(5:31-5:32),OpenCurly(5:32-5:33),CloseCurly(5:33-5:34),Comma(5:34-5:35),Underscore(5:36-5:37),CloseRound(5:37-5:38),
LowerIdent(6:1-6:10),OpColon(6:11-6:12),UpperIdent(6:13-6:18),NoSpaceOpenRound(6:18-6:19),NoSpaceOpenRound(6:19-6:20),NamedUnderscore(6:20-6:22),Comma(6:22-6:23),NamedUnderscore(6:24-6:26),Comma(6:26-6:27),NamedUnderscore(6:28-6:30),CloseRound(6:30-6:31),CloseRound(6:31-6:32),
EndOfFile(7:1-7:1),
~~~
# PARSE
~~~clojure
(file @1.1-6.32
	(type-module @1.1-1.4)
	(statements
		(s-type-anno @1.1-1.10 (name "foo")
			(ty @1.7-1.10 (name "U64")))
		(s-type-anno @2.1-2.23 (name "bar")
			(ty-apply @2.7-2.23
				(ty @2.7-2.12 (name "Thing"))
				(underscore-ty-var @2.13-2.15 (raw "_a"))
				(underscore-ty-var @2.17-2.19 (raw "_b"))
				(_)))
		(s-type-anno @3.1-3.19 (name "baz")
			(ty-tuple @3.7-3.19
				(underscore-ty-var @3.8-3.10 (raw "_a"))
				(underscore-ty-var @3.12-3.14 (raw "_b"))
				(underscore-ty-var @3.16-3.18 (raw "_c"))))
		(s-type-anno @4.1-4.27 (name "add_one")
			(ty-fn @4.12-4.26
				(ty @4.12-4.14 (name "U8"))
				(ty @4.16-4.19 (name "U16"))
				(ty @4.23-4.26 (name "U32"))))
		(s-type-anno @5.1-5.38 (name "main!")
			(ty-fn @5.9-5.38
				(ty-apply @5.9-5.21
					(ty @5.9-5.13 (name "List"))
					(ty @5.14-5.20 (name "String")))
				(ty-apply @5.25-5.38
					(ty @5.25-5.31 (name "Result"))
					(ty-record @5.32-5.34)
					(_))))
		(s-type-anno @6.1-6.32 (name "tag_tuple")
			(ty-apply @6.13-6.32
				(ty @6.13-6.18 (name "Value"))
				(ty-tuple @6.19-6.31
					(underscore-ty-var @6.20-6.22 (raw "_a"))
					(underscore-ty-var @6.24-6.26 (raw "_b"))
					(underscore-ty-var @6.28-6.30 (raw "_c")))))))
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

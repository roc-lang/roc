# META
~~~ini
description=fuzz crash
type=file:FuzzCrash066.roc
~~~
# SOURCE
~~~roc
FuzzCrash066 := {}

C:[0]
~~~
# EXPECTED
UNEXPECTED TOKEN IN TYPE ANNOTATION - fuzz_crash_066.md:3:4:3:5
MALFORMED TYPE - fuzz_crash_066.md:3:4:3:5
# PROBLEMS
**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **0** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

**fuzz_crash_066.md:3:4:3:5:**
```roc
C:[0]
```
   ^


**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

**fuzz_crash_066.md:3:4:3:5:**
```roc
C:[0]
```
   ^


# TOKENS
~~~zig
UpperIdent(1:1-1:13),OpColonEqual(1:14-1:16),OpenCurly(1:17-1:18),CloseCurly(1:18-1:19),
UpperIdent(3:1-3:2),OpColon(3:2-3:3),OpenSquare(3:3-3:4),Int(3:4-3:5),CloseSquare(3:5-3:6),
EndOfFile(4:1-4:1),
~~~
# PARSE
~~~clojure
(file @1.1-3.6
	(type-module @1.1-1.13)
	(statements
		(s-type-decl @1.1-1.19
			(header @1.1-1.13 (name "FuzzCrash066")
				(args))
			(ty-record @1.17-1.19))
		(s-type-decl @3.1-3.6
			(header @3.1-3.2 (name "C")
				(args))
			(ty-tag-union @3.3-3.6
				(tags
					(ty-malformed @3.4-3.5 (tag "ty_anno_unexpected_token")))))))
~~~
# FORMATTED
~~~roc
FuzzCrash066 := {}

C : []
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-nominal-decl @1.1-1.19
		(ty-header @1.1-1.13 (name "FuzzCrash066"))
		(ty-record @1.17-1.19))
	(s-alias-decl @3.1-3.6
		(ty-header @3.1-3.2 (name "C"))
		(ty-tag-union @3.3-3.6
			(ty-malformed @3.4-3.5))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(nominal @1.1-1.19 (type "FuzzCrash066")
			(ty-header @1.1-1.13 (name "FuzzCrash066")))
		(alias @3.1-3.6 (type "C")
			(ty-header @3.1-3.2 (name "C"))))
	(expressions))
~~~

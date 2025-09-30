# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
C:[0]
~~~
# EXPECTED
UNEXPECTED TOKEN IN TYPE ANNOTATION - fuzz_crash_066.md:1:4:1:5
MALFORMED TYPE - fuzz_crash_066.md:1:4:1:5
TYPE MODULE MISSING MATCHING TYPE - fuzz_crash_066.md:1:1:1:6
# PROBLEMS
**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **0** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

**fuzz_crash_066.md:1:4:1:5:**
```roc
C:[0]
```
   ^


**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

**fuzz_crash_066.md:1:4:1:5:**
```roc
C:[0]
```
   ^


**TYPE MODULE MISSING MATCHING TYPE**
Type modules must have a type declaration matching the module name.

This file is named `fuzz_crash_066.roc`, but no top-level type declaration named `fuzz_crash_066` was found.

Add either:
`fuzz_crash_066 := ...` (nominal type)
or:
`fuzz_crash_066 : ...` (type alias)
**fuzz_crash_066.md:1:1:1:6:**
```roc
C:[0]
```
^^^^^


# TOKENS
~~~zig
UpperIdent(1:1-1:2),OpColon(1:2-1:3),OpenSquare(1:3-1:4),Int(1:4-1:5),CloseSquare(1:5-1:6),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.6
	(type-module @1.1-1.2)
	(statements
		(s-type-decl @1.1-1.6
			(header @1.1-1.2 (name "C")
				(args))
			(ty-tag-union @1.3-1.6
				(tags
					(ty-malformed @1.4-1.5 (tag "ty_anno_unexpected_token")))))))
~~~
# FORMATTED
~~~roc
C : []
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl @1.1-1.6
		(ty-header @1.1-1.2 (name "C"))
		(ty-tag-union @1.3-1.6
			(ty-malformed @1.4-1.5))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias @1.1-1.6 (type "C")
			(ty-header @1.1-1.2 (name "C"))))
	(expressions))
~~~

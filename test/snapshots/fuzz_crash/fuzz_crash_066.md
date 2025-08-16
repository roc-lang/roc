# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module []

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
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
UpperIdent(3:1-3:2),OpColon(3:2-3:3),OpenSquare(3:3-3:4),Int(3:4-3:5),CloseSquare(3:5-3:6),EndOfFile(3:6-3:6),
~~~
# PARSE
~~~clojure
(file @1.1-3.6
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-type-decl @3.1-3.6
			(header @3.1-3.2 (name "C")
				(args))
			(ty-tag-union @3.3-3.6
				(tags
					(ty-malformed @3.4-3.5 (tag "ty_anno_unexpected_token")))))))
~~~
# FORMATTED
~~~roc
module []

C : []
~~~
# CANONICALIZE
~~~clojure
(can-ir
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
		(alias @3.1-3.6 (type "C")
			(ty-header @3.1-3.2 (name "C"))))
	(expressions))
~~~

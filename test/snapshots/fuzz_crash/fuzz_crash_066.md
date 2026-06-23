# META
~~~ini
description=fuzz crash
type=snippet
~~~
# SOURCE
~~~roc
C:[0]
~~~
# EXPECTED
UNEXPECTED TOKEN IN TYPE ANNOTATION - fuzz_crash_066.md:1:4:1:5
MALFORMED TYPE - fuzz_crash_066.md:1:4:1:5
# PROBLEMS

┌─────────────────────────────────────┐
│ UNEXPECTED TOKEN IN TYPE ANNOTATION ├─ The token 0 is not expected in a ────┐
└┬────────────────────────────────────┘  type annotation.                     │
 │                                                                            │
 │  C:[0]                                                                     │
 │     ‾                                                                      │
 └───────────────────────────────────────────────────── fuzz_crash_066.md:1:4 ┘

    Type annotations should contain types like Str, Num a, or List U64.


┌────────────────┐
│ MALFORMED TYPE ├─ This type annotation is malformed or contains invalid ────┐
└┬───────────────┘  syntax.                                                   │
 │                                                                            │
 │  C:[0]                                                                     │
 │     ‾                                                                      │
 └───────────────────────────────────────────────────── fuzz_crash_066.md:1:4 ┘


# TOKENS
~~~zig
UpperIdent,OpColon,OpenSquare,Int,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "C")
				(args))
			(ty-tag-union
				(tags
					(ty-malformed (tag "ty_anno_unexpected_token")))))))
~~~
# FORMATTED
~~~roc
C : []
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl
		(ty-header (name "C"))
		(ty-tag-union
			(ty-malformed))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias (type "C")
			(ty-header (name "C"))))
	(expressions))
~~~

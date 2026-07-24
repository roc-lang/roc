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
UNEXPECTED TYPE SYNTAX - fuzz_crash_066.md:1:4:1:5
MALFORMED TYPE - fuzz_crash_066.md:1:4:1:5
# PROBLEMS

┌────────────────────────┐
│ UNEXPECTED TYPE SYNTAX ├─ I was parsing a type annotation, and this token ──┐
└┬───────────────────────┘  cannot start a type here.                         │
 │                                                                            │
 │  C:[0]                                                                     │
 │     ‾                                                                      │
 └───────────────────────────────────────────────────── fuzz_crash_066.md:1:4 ┘

    Types can be type variables, uppercase type names, function types, tuples,
    records, or tag unions.

    For example:
        List(U64)

    I found `0` here.


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
	(type-mod)
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

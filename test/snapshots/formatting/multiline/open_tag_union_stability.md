# META
~~~ini
description=Formatter stability for open tag unions with blank lines before ..
type=snippet
~~~
# SOURCE
~~~roc
r : [
	a,

	..,
]
~~~
# EXPECTED
MALFORMED TYPE - open_tag_union_stability.md:2:2:2:3
DECLARATION HAS NO VALUE - open_tag_union_stability.md:1:1:5:2
# PROBLEMS

┌────────────────┐
│ MALFORMED TYPE ├─ This type annotation is malformed or contains invalid ────┐
└┬───────────────┘  syntax.                                                   │
 │                                                                            │
 │  a,                                                                        │
 │  ‾                                                                         │
 └─────────────────────────────────────────── open_tag_union_stability.md:2:2 ┘



┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  r : [                                                                     │
 │      a,                                                                    │
 │                                                                            │
 │      ..,                                                                   │
 │  ]                                                                         │
 │                                                                            │
 └─────────────────────────────────────────── open_tag_union_stability.md:1:1 ┘

    Add a value body here, or put hosted functions in a platform type module so
    they are published through the host boundary.

# TOKENS
~~~zig
LowerIdent,OpColon,OpenSquare,
LowerIdent,Comma,
DoubleDot,Comma,
CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "r")
			(ty-tag-union
				(tags
					(ty-var (raw "a")))
				..))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "r"))
		(e-anno-only)
		(annotation
			(ty-tag-union
				(ty-malformed)
				(ty-rigid-var (name "#others"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~

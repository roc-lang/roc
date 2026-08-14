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
── ✗ malformed type ──────────────────────────── open_tag_union_stability.md:2:2

This type annotation is malformed or contains invalid syntax.

a,
^

── ● declaration has no value ────────────────── open_tag_union_stability.md:1:1

This declaration has a type annotation but no implementation.

r : [
    a,

    ..,
]

Add a value body here, or put hosted functions in a platform type mod so
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
	(type-mod)
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
		(e-runtime-error (tag "erroneous_value_expr"))
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

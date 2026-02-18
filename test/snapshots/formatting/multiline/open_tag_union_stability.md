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
# PROBLEMS
**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

**open_tag_union_stability.md:2:2:2:3:**
```roc
	a,
```
	^


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
				(ty-underscore)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~

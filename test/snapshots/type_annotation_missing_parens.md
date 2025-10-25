# META
~~~ini
description=Type annotation missing parentheses for type application
type=snippet
~~~
# SOURCE
~~~roc
nums : List U8
~~~
# EXPECTED
PARSE ERROR - type_annotation_missing_parens.md:2:1:2:1
# PROBLEMS
**PARSE ERROR**
Type applications require parentheses around their type arguments.

I found a type followed by what looks like a type argument, but they need to be connected with parentheses.

Instead of:
    **List U8**

Use:
    **List(U8)**

Other valid examples:
    `Dict(Str, Num)`
    `Result(a, Str)`
    `Maybe(List(U64))`

**type_annotation_missing_parens.md:2:1:2:1:**
```roc

```
^


# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,UpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "nums")
			(ty (name "List")))
		(s-malformed (tag "expected_colon_after_type_annotation"))))
~~~
# FORMATTED
~~~roc
nums : List
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-type-anno (name "nums")
		(ty-lookup (name "List") (builtin))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~

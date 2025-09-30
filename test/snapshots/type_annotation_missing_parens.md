# META
~~~ini
description=Type annotation missing parentheses for type application
type=file
~~~
# SOURCE
~~~roc
nums : List U8
~~~
# EXPECTED
PARSE ERROR - type_annotation_missing_parens.md:2:1:2:1
MISSING MAIN! FUNCTION - type_annotation_missing_parens.md:1:1:1:15
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


**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**type_annotation_missing_parens.md:1:1:1:15:**
```roc
nums : List U8
```
^^^^^^^^^^^^^^


# TOKENS
~~~zig
LowerIdent(1:1-1:5),OpColon(1:6-1:7),UpperIdent(1:8-1:12),UpperIdent(1:13-1:15),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.15
	(type-module @1.1-1.5)
	(statements
		(s-type-anno @1.1-1.12 (name "nums")
			(ty @1.8-1.12 (name "List")))
		(s-malformed @1.1-1.1 (tag "expected_colon_after_type_annotation"))))
~~~
# FORMATTED
~~~roc
nums : List
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

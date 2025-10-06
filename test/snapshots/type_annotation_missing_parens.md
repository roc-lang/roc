# META
~~~ini
description=Type annotation missing parentheses for type application
type=file:TypeAnnotationMissingParens.roc
~~~
# SOURCE
~~~roc
TypeAnnotationMissingParens := {}

nums : List U8
~~~
# EXPECTED
PARSE ERROR - type_annotation_missing_parens.md:4:1:4:1
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

**type_annotation_missing_parens.md:4:1:4:1:**
```roc

```
^


# TOKENS
~~~zig
UpperIdent(1:1-1:28),OpColonEqual(1:29-1:31),OpenCurly(1:32-1:33),CloseCurly(1:33-1:34),
LowerIdent(3:1-3:5),OpColon(3:6-3:7),UpperIdent(3:8-3:12),UpperIdent(3:13-3:15),
EndOfFile(4:1-4:1),
~~~
# PARSE
~~~clojure
(file @1.1-3.15
	(type-module @1.1-1.28)
	(statements
		(s-type-decl @1.1-1.34
			(header @1.1-1.28 (name "TypeAnnotationMissingParens")
				(args))
			(ty-record @1.32-1.34))
		(s-type-anno @3.1-3.12 (name "nums")
			(ty @3.8-3.12 (name "List")))
		(s-malformed @1.1-1.1 (tag "expected_colon_after_type_annotation"))))
~~~
# FORMATTED
~~~roc
TypeAnnotationMissingParens := {}

nums : List
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-nominal-decl @1.1-1.34
		(ty-header @1.1-1.28 (name "TypeAnnotationMissingParens"))
		(ty-record @1.32-1.34)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(nominal @1.1-1.34 (type "TypeAnnotationMissingParens")
			(ty-header @1.1-1.28 (name "TypeAnnotationMissingParens"))))
	(expressions))
~~~

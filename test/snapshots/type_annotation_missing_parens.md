# META
~~~ini
description=Type annotation missing parentheses for type application
type=file
~~~
# SOURCE
~~~roc
module [nums]

nums : List U8
~~~
# EXPECTED
PARSE ERROR - type_annotation_missing_parens.md:4:1:4:1
MODULE HEADER DEPRECATED - type_annotation_missing_parens.md:1:1:1:14
EXPOSED BUT NOT DEFINED - type_annotation_missing_parens.md:1:9:1:13
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


**MODULE HEADER DEPRECATED**
The `module` header is deprecated.

Type modules (headerless files with a top-level type matching the filename) are now the preferred way to define modules.

Remove the `module` header and ensure your file defines a type that matches the filename.
**type_annotation_missing_parens.md:1:1:1:14:**
```roc
module [nums]
```
^^^^^^^^^^^^^


**EXPOSED BUT NOT DEFINED**
The module header says that `nums` is exposed, but it is not defined anywhere in this module.

**type_annotation_missing_parens.md:1:9:1:13:**
```roc
module [nums]
```
        ^^^^
You can fix this by either defining `nums` in this module, or by removing it from the list of exposed values.

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:13),CloseSquare(1:13-1:14),
LowerIdent(3:1-3:5),OpColon(3:6-3:7),UpperIdent(3:8-3:12),UpperIdent(3:13-3:15),
EndOfFile(4:1-4:1),
~~~
# PARSE
~~~clojure
(file @1.1-3.15
	(module @1.1-1.14
		(exposes @1.8-1.14
			(exposed-lower-ident @1.9-1.13
				(text "nums"))))
	(statements
		(s-type-anno @3.1-3.12 (name "nums")
			(ty @3.8-3.12 (name "List")))
		(s-malformed @1.1-1.1 (tag "expected_colon_after_type_annotation"))))
~~~
# FORMATTED
~~~roc
module [nums]

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

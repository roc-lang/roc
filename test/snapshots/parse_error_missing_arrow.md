# META
~~~ini
description=Type annotation with missing arrow
type=file
~~~
# SOURCE
~~~roc
module [main]

foo : Str Num
~~~
# EXPECTED
PARSE ERROR - parse_error_missing_arrow.md:4:1:4:1
MODULE HEADER DEPRECATED - parse_error_missing_arrow.md:1:1:1:14
EXPOSED BUT NOT DEFINED - parse_error_missing_arrow.md:1:9:1:13
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
    `Try(a, Str)`
    `Maybe(List(U64))`

**parse_error_missing_arrow.md:4:1:4:1:**
```roc

```
^


**MODULE HEADER DEPRECATED**
The `module` header is deprecated.

Type modules (headerless files with a top-level type matching the filename) are now the preferred way to define modules.

Remove the `module` header and ensure your file defines a type that matches the filename.
**parse_error_missing_arrow.md:1:1:1:14:**
```roc
module [main]
```
^^^^^^^^^^^^^


**EXPOSED BUT NOT DEFINED**
The module header says that `main` is exposed, but it is not defined anywhere in this module.

**parse_error_missing_arrow.md:1:9:1:13:**
```roc
module [main]
```
        ^^^^
You can fix this by either defining `main` in this module, or by removing it from the list of exposed values.

# TOKENS
~~~zig
KwModule,OpenSquare,LowerIdent,CloseSquare,
LowerIdent,OpColon,UpperIdent,UpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(module
		(exposes
			(exposed-lower-ident
				(text "main"))))
	(statements
		(s-type-anno (name "foo")
			(ty (name "Str")))
		(s-malformed (tag "expected_colon_after_type_annotation"))))
~~~
# FORMATTED
~~~roc
module [main]

foo : Str
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "foo"))
		(e-anno-only)
		(annotation
			(ty-lookup (name "Str") (builtin)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str")))
	(expressions
		(expr (type "Str"))))
~~~

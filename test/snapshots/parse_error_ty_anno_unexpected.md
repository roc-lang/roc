# META
~~~ini
description=Type annotation with unexpected token
type=file
~~~
# SOURCE
~~~roc
module [main]

foo : + Str
~~~
# EXPECTED
UNEXPECTED TOKEN IN TYPE ANNOTATION - parse_error_ty_anno_unexpected.md:3:7:3:8
PARSE ERROR - parse_error_ty_anno_unexpected.md:4:1:4:1
MODULE HEADER DEPRECATED - parse_error_ty_anno_unexpected.md:1:1:1:14
MALFORMED TYPE - parse_error_ty_anno_unexpected.md:3:7:3:8
EXPOSED BUT NOT DEFINED - parse_error_ty_anno_unexpected.md:1:9:1:13
# PROBLEMS
**UNEXPECTED TOKEN IN TYPE ANNOTATION**
The token **+** is not expected in a type annotation.
Type annotations should contain types like _Str_, _Num a_, or _List U64_.

**parse_error_ty_anno_unexpected.md:3:7:3:8:**
```roc
foo : + Str
```
      ^


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

**parse_error_ty_anno_unexpected.md:4:1:4:1:**
```roc

```
^


**MODULE HEADER DEPRECATED**
The `module` header is deprecated.

Type modules (headerless files with a top-level type matching the filename) are now the preferred way to define modules.

Remove the `module` header and ensure your file defines a type that matches the filename.
**parse_error_ty_anno_unexpected.md:1:1:1:14:**
```roc
module [main]
```
^^^^^^^^^^^^^


**MALFORMED TYPE**
This type annotation is malformed or contains invalid syntax.

**parse_error_ty_anno_unexpected.md:3:7:3:8:**
```roc
foo : + Str
```
      ^


**EXPOSED BUT NOT DEFINED**
The module header says that `main` is exposed, but it is not defined anywhere in this module.

**parse_error_ty_anno_unexpected.md:1:9:1:13:**
```roc
module [main]
```
        ^^^^
You can fix this by either defining `main` in this module, or by removing it from the list of exposed values.

# TOKENS
~~~zig
KwModule,OpenSquare,LowerIdent,CloseSquare,
LowerIdent,OpColon,OpPlus,UpperIdent,
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
			(ty-malformed (tag "ty_anno_unexpected_token")))
		(s-malformed (tag "expected_colon_after_type_annotation"))))
~~~
# FORMATTED
~~~roc
module [main]

foo : 
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "foo"))
		(e-anno-only)
		(annotation
			(ty-malformed))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~

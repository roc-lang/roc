# META
~~~ini
description=Type declaration with EOF in parentheses
type=file
~~~
# SOURCE
~~~roc
module [main]

Foo(a
~~~
# EXPECTED
PARSE ERROR - parse_error_type_decl_eof.md:3:1:3:4
PARSE ERROR - parse_error_type_decl_eof.md:4:1:4:1
MODULE HEADER DEPRECATED - parse_error_type_decl_eof.md:1:1:1:14
EXPOSED BUT NOT DEFINED - parse_error_type_decl_eof.md:1:9:1:13
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_ty_anno_close_round_or_comma`
This is an unexpected parsing error. Please check your syntax.

**parse_error_type_decl_eof.md:3:1:3:4:**
```roc
Foo(a
```
^^^


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

**parse_error_type_decl_eof.md:4:1:4:1:**
```roc

```
^


**MODULE HEADER DEPRECATED**
The `module` header is deprecated.

Type modules (headerless files with a top-level type matching the filename) are now the preferred way to define modules.

Remove the `module` header and ensure your file defines a type that matches the filename.
**parse_error_type_decl_eof.md:1:1:1:14:**
```roc
module [main]
```
^^^^^^^^^^^^^


**EXPOSED BUT NOT DEFINED**
The module header says that `main` is exposed, but it is not defined anywhere in this module.

**parse_error_type_decl_eof.md:1:9:1:13:**
```roc
module [main]
```
        ^^^^
You can fix this by either defining `main` in this module, or by removing it from the list of exposed values.

# TOKENS
~~~zig
KwModule,OpenSquare,LowerIdent,CloseSquare,
UpperIdent,NoSpaceOpenRound,LowerIdent,
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
		(s-malformed (tag "expected_colon_after_type_annotation"))))
~~~
# FORMATTED
~~~roc
module [main]
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

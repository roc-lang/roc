# META
~~~ini
description=Exposed upper ident with invalid 'as' name
type=file
~~~
# SOURCE
~~~roc
module [Foo as 123]
~~~
# EXPECTED
PARSE ERROR - parse_error_exposed_upper_as.md:1:9:1:12
MODULE HEADER DEPRECATED - parse_error_exposed_upper_as.md:1:1:1:20
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_upper_name_after_exposed_item_as`
This is an unexpected parsing error. Please check your syntax.

**parse_error_exposed_upper_as.md:1:9:1:12:**
```roc
module [Foo as 123]
```
        ^^^


**MODULE HEADER DEPRECATED**
The `module` header is deprecated.

Type modules (headerless files with a top-level type matching the filename) are now the preferred way to define modules.

Remove the `module` header and ensure your file defines a type that matches the filename.
**parse_error_exposed_upper_as.md:1:1:1:20:**
```roc
module [Foo as 123]
```
^^^^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
KwModule,OpenSquare,UpperIdent,KwAs,Int,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(module
		(exposes
			(exposed-malformed (reason "expected_upper_name_after_exposed_item_as"))))
	(statements))
~~~
# FORMATTED
~~~roc
module []
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

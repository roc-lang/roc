# META
~~~ini
description=Exposed lower ident with invalid 'as' name
type=file
~~~
# SOURCE
~~~roc
module [foo as 123]
~~~
# EXPECTED
PARSE ERROR - parse_error_exposed_lower_as.md:1:9:1:12
MODULE HEADER DEPRECATED - parse_error_exposed_lower_as.md:1:1:1:20
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_lower_name_after_exposed_item_as`
This is an unexpected parsing error. Please check your syntax.

**parse_error_exposed_lower_as.md:1:9:1:12:**
```roc
module [foo as 123]
```
        ^^^


**MODULE HEADER DEPRECATED**
The `module` header is deprecated.

Type modules (headerless files with a top-level type matching the filename) are now the preferred way to define modules.

Remove the `module` header and ensure your file defines a type that matches the filename.
**parse_error_exposed_lower_as.md:1:1:1:20:**
```roc
module [foo as 123]
```
^^^^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
KwModule,OpenSquare,LowerIdent,KwAs,Int,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(module
		(exposes
			(exposed-malformed (reason "expected_lower_name_after_exposed_item_as"))))
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

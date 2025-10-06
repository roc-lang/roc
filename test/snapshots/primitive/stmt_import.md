# META
~~~ini
description=A primitive
type=snippet
~~~
# SOURCE
~~~roc
import json.Json [foo, BAR]
~~~
# EXPECTED
PARSE ERROR - stmt_import.md:1:18:1:19
PARSE ERROR - stmt_import.md:1:19:1:22
PARSE ERROR - stmt_import.md:1:22:1:23
PARSE ERROR - stmt_import.md:1:27:1:28
MODULE NOT FOUND - stmt_import.md:1:1:1:17
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**stmt_import.md:1:18:1:19:**
```roc
import json.Json [foo, BAR]
```
                 ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**stmt_import.md:1:19:1:22:**
```roc
import json.Json [foo, BAR]
```
                  ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**stmt_import.md:1:22:1:23:**
```roc
import json.Json [foo, BAR]
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
    `Result(a, Str)`
    `Maybe(List(U64))`

**stmt_import.md:1:27:1:28:**
```roc
import json.Json [foo, BAR]
```
                          ^


**MODULE NOT FOUND**
The module `json.Json` was not found in this Roc project.

You're attempting to use this module here:
**stmt_import.md:1:1:1:17:**
```roc
import json.Json [foo, BAR]
```
^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
KwImport(1:1-1:7),LowerIdent(1:8-1:12),NoSpaceDotUpperIdent(1:12-1:17),OpenSquare(1:18-1:19),LowerIdent(1:19-1:22),Comma(1:22-1:23),UpperIdent(1:24-1:27),CloseSquare(1:27-1:28),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.28
	(type-module @1.1-1.7)
	(statements
		(s-import @1.1-1.17 (raw "json.Json"))
		(s-malformed @1.18-1.19 (tag "statement_unexpected_token"))
		(s-malformed @1.19-1.22 (tag "statement_unexpected_token"))
		(s-malformed @1.22-1.23 (tag "statement_unexpected_token"))
		(s-malformed @1.27-1.28 (tag "expected_colon_after_type_annotation"))))
~~~
# FORMATTED
~~~roc
import json.Json
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-import @1.1-1.17 (module "json.Json") (qualifier "json")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~

# META
~~~ini
description=A primitive
type=file
~~~
# SOURCE
~~~roc
module []

import json.Json [foo, BAR]
~~~
# EXPECTED
PARSE ERROR - stmt_import.md:3:18:3:19
PARSE ERROR - stmt_import.md:3:19:3:22
PARSE ERROR - stmt_import.md:3:22:3:23
PARSE ERROR - stmt_import.md:3:27:3:28
MODULE HEADER DEPRECATED - stmt_import.md:1:1:1:10
MODULE NOT FOUND - stmt_import.md:3:1:3:17
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**stmt_import.md:3:18:3:19:**
```roc
import json.Json [foo, BAR]
```
                 ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**stmt_import.md:3:19:3:22:**
```roc
import json.Json [foo, BAR]
```
                  ^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**stmt_import.md:3:22:3:23:**
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

**stmt_import.md:3:27:3:28:**
```roc
import json.Json [foo, BAR]
```
                          ^


**MODULE HEADER DEPRECATED**
The `module` header is deprecated.

Type modules (headerless files with a top-level type matching the filename) are now the preferred way to define modules.

Remove the `module` header and ensure your file defines a type that matches the filename.
**stmt_import.md:1:1:1:10:**
```roc
module []
```
^^^^^^^^^


**MODULE NOT FOUND**
The module `json.Json` was not found in this Roc project.

You're attempting to use this module here:
**stmt_import.md:3:1:3:17:**
```roc
import json.Json [foo, BAR]
```
^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
KwImport(3:1-3:7),LowerIdent(3:8-3:12),NoSpaceDotUpperIdent(3:12-3:17),OpenSquare(3:18-3:19),LowerIdent(3:19-3:22),Comma(3:22-3:23),UpperIdent(3:24-3:27),CloseSquare(3:27-3:28),
EndOfFile(4:1-4:1),
~~~
# PARSE
~~~clojure
(file @1.1-3.28
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-import @3.1-3.17 (raw "json.Json"))
		(s-malformed @3.18-3.19 (tag "statement_unexpected_token"))
		(s-malformed @3.19-3.22 (tag "statement_unexpected_token"))
		(s-malformed @3.22-3.23 (tag "statement_unexpected_token"))
		(s-malformed @3.27-3.28 (tag "expected_colon_after_type_annotation"))))
~~~
# FORMATTED
~~~roc
module []

import json.Json
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-import @3.1-3.17 (module "json.Json") (qualifier "json")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~

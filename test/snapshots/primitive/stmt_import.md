# META
~~~ini
description=A primitive
type=file:StmtImport.roc
~~~
# SOURCE
~~~roc
StmtImport := {}

import json.Json [foo, BAR]
~~~
# EXPECTED
PARSE ERROR - stmt_import.md:3:18:3:19
PARSE ERROR - stmt_import.md:3:19:3:22
PARSE ERROR - stmt_import.md:3:22:3:23
PARSE ERROR - stmt_import.md:3:27:3:28
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
UpperIdent(1:1-1:11),OpColonEqual(1:12-1:14),OpenCurly(1:15-1:16),CloseCurly(1:16-1:17),
KwImport(3:1-3:7),LowerIdent(3:8-3:12),NoSpaceDotUpperIdent(3:12-3:17),OpenSquare(3:18-3:19),LowerIdent(3:19-3:22),Comma(3:22-3:23),UpperIdent(3:24-3:27),CloseSquare(3:27-3:28),
EndOfFile(4:1-4:1),
~~~
# PARSE
~~~clojure
(file @1.1-3.28
	(type-module @1.1-1.11)
	(statements
		(s-type-decl @1.1-1.17
			(header @1.1-1.11 (name "StmtImport")
				(args))
			(ty-record @1.15-1.17))
		(s-import @3.1-3.17 (raw "json.Json"))
		(s-malformed @3.18-3.19 (tag "statement_unexpected_token"))
		(s-malformed @3.19-3.22 (tag "statement_unexpected_token"))
		(s-malformed @3.22-3.23 (tag "statement_unexpected_token"))
		(s-malformed @3.27-3.28 (tag "expected_colon_after_type_annotation"))))
~~~
# FORMATTED
~~~roc
StmtImport := {}

import json.Json
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-nominal-decl @1.1-1.17
		(ty-header @1.1-1.11 (name "StmtImport"))
		(ty-record @1.15-1.17))
	(s-import @3.1-3.17 (module "json.Json") (qualifier "json")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(nominal @1.1-1.17 (type "StmtImport")
			(ty-header @1.1-1.11 (name "StmtImport"))))
	(expressions))
~~~

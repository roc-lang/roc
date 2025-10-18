# META
~~~ini
description=inline_ingested_file
type=snippet
~~~
# SOURCE
~~~roc
import "users.json" as data : Str
import Json

foo = Json.parse(data)
~~~
# EXPECTED
PARSE ERROR - inline_ingested_file.md:1:8:1:9
PARSE ERROR - inline_ingested_file.md:1:9:1:19
PARSE ERROR - inline_ingested_file.md:1:19:1:20
PARSE ERROR - inline_ingested_file.md:1:21:1:23
MODULE NOT FOUND - inline_ingested_file.md:2:1:2:12
UNDEFINED VARIABLE - inline_ingested_file.md:4:18:4:22
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `incomplete_import`
This is an unexpected parsing error. Please check your syntax.

**inline_ingested_file.md:1:8:1:9:**
```roc
import "users.json" as data : Str
```
       ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**inline_ingested_file.md:1:9:1:19:**
```roc
import "users.json" as data : Str
```
        ^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**inline_ingested_file.md:1:19:1:20:**
```roc
import "users.json" as data : Str
```
                  ^


**PARSE ERROR**
A parsing error occurred: `statement_unexpected_token`
This is an unexpected parsing error. Please check your syntax.

**inline_ingested_file.md:1:21:1:23:**
```roc
import "users.json" as data : Str
```
                    ^^


**MODULE NOT FOUND**
The module `Json` was not found in this Roc project.

You're attempting to use this module here:
**inline_ingested_file.md:2:1:2:12:**
```roc
import Json
```
^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `data` in this scope.
Is there an `import` or `exposing` missing up-top?

**inline_ingested_file.md:4:18:4:22:**
```roc
foo = Json.parse(data)
```
                 ^^^^


# TOKENS
~~~zig
KwImport,StringStart,StringPart,StringEnd,KwAs,LowerIdent,OpColon,UpperIdent,
KwImport,UpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-malformed (tag "incomplete_import"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-malformed (tag "statement_unexpected_token"))
		(s-type-anno (name "data")
			(ty (name "Str")))
		(s-import (raw "Json"))
		(s-decl
			(p-ident (raw "foo"))
			(e-apply
				(e-ident (raw "Json.parse"))
				(e-ident (raw "data"))))))
~~~
# FORMATTED
~~~roc
data : Str
import Json

foo = Json.parse(data)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "foo"))
		(e-call
			(e-lookup-external
				(module-idx "2")
				(target-node-idx "0"))
			(e-runtime-error (tag "ident_not_in_scope"))))
	(s-import (module "Json")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_a")))
	(expressions
		(expr (type "_a"))))
~~~

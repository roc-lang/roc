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
FILE NOT FOUND - inline_ingested_file.md:1:1:1:34
UNDEFINED VARIABLE - inline_ingested_file.md:4:7:4:17
# PROBLEMS
**FILE IMPORT ERROR**
Could not read the file **users.json**.
An IO error occurred while trying to read this file:
**inline_ingested_file.md:1:1:1:34:**
```roc
import "users.json" as data : Str
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `parse` in this scope.
Is there an `import` or `exposing` missing up-top?

**inline_ingested_file.md:4:7:4:17:**
```roc
foo = Json.parse(data)
```
      ^^^^^^^^^^


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
		(s-file-import
			(path "users.json")
			(name "data")
			(type "Str"))
		(s-import (raw "Json"))
		(s-decl
			(p-ident (raw "foo"))
			(e-apply
				(e-ident (raw "Json.parse"))
				(e-ident (raw "data"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "data"))
		(e-runtime-error (tag "file_import_io_error")))
	(d-let
		(p-assign (ident "foo"))
		(e-call
			(e-runtime-error (tag "ident_not_in_scope"))
			(e-lookup-local
				(p-assign (ident "data")))))
	(s-import (module "Json")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error"))
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))
		(expr (type "Error"))))
~~~

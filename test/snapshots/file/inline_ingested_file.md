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
MODULE NOT FOUND - inline_ingested_file.md:2:1:2:12
UNDEFINED VARIABLE - inline_ingested_file.md:4:7:4:17
# PROBLEMS
**FILE NOT FOUND**
The file **users.json** was not found.

Make sure the file exists relative to your source file:
**inline_ingested_file.md:1:1:1:34:**
```roc
import "users.json" as data : Str
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**MODULE NOT FOUND**
The module `Json` was not found in this Roc project.

You're attempting to use this module here:
**inline_ingested_file.md:2:1:2:12:**
```roc
import Json
```
^^^^^^^^^^^


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
		(e-runtime-error (tag "file_import_not_found")))
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

# META
~~~ini
description=Import with module-qualified usage
type=file:CanImportJson.roc
~~~
# SOURCE
~~~roc
CanImportJson := {}

import json.Json

main = Json.utf8
~~~
# EXPECTED
MODULE NOT FOUND - can_import_json.md:3:1:3:17
# PROBLEMS
**MODULE NOT FOUND**
The module `json.Json` was not found in this Roc project.

You're attempting to use this module here:
**can_import_json.md:3:1:3:17:**
```roc
import json.Json
```
^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
UpperIdent(1:1-1:14),OpColonEqual(1:15-1:17),OpenCurly(1:18-1:19),CloseCurly(1:19-1:20),
KwImport(3:1-3:7),LowerIdent(3:8-3:12),NoSpaceDotUpperIdent(3:12-3:17),
LowerIdent(5:1-5:5),OpAssign(5:6-5:7),UpperIdent(5:8-5:12),NoSpaceDotLowerIdent(5:12-5:17),
EndOfFile(6:1-6:1),
~~~
# PARSE
~~~clojure
(file @1.1-5.17
	(type-module @1.1-1.14)
	(statements
		(s-type-decl @1.1-1.20
			(header @1.1-1.14 (name "CanImportJson")
				(args))
			(ty-record @1.18-1.20))
		(s-import @3.1-3.17 (raw "json.Json"))
		(s-decl @5.1-5.17
			(p-ident @5.1-5.5 (raw "main"))
			(e-ident @5.8-5.17 (raw "Json.utf8")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @5.1-5.5 (ident "main"))
		(e-lookup-external @5.8-5.17
			(module-idx "0")
			(target-node-idx "0")))
	(s-nominal-decl @1.1-1.20
		(ty-header @1.1-1.14 (name "CanImportJson"))
		(ty-record @1.18-1.20))
	(s-import @3.1-3.17 (module "json.Json") (qualifier "json")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @5.1-5.5 (type "Error")))
	(type_decls
		(nominal @1.1-1.20 (type "CanImportJson")
			(ty-header @1.1-1.14 (name "CanImportJson"))))
	(expressions
		(expr @5.8-5.17 (type "Error"))))
~~~

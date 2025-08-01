# META
~~~ini
description=Import with module-qualified usage
type=file
~~~
# SOURCE
~~~roc
module []

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
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
KwImport(3:1-3:7),LowerIdent(3:8-3:12),NoSpaceDotUpperIdent(3:12-3:17),
LowerIdent(5:1-5:5),OpAssign(5:6-5:7),UpperIdent(5:8-5:12),NoSpaceDotLowerIdent(5:12-5:17),EndOfFile(5:17-5:17),
~~~
# PARSE
~~~clojure
(file @1.1-5.17
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
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
	(s-import @3.1-3.17 (module "json.Json") (qualifier "json")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @5.1-5.5 (type "Error")))
	(expressions
		(expr @5.8-5.17 (type "Error"))))
~~~

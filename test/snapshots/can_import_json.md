# META
~~~ini
description=Import with module-qualified usage
type=snippet
~~~
# SOURCE
~~~roc
import json.Json

main = Json.utf8
~~~
# EXPECTED
MODULE NOT FOUND - can_import_json.md:1:1:1:17
# PROBLEMS
**MODULE NOT FOUND**
The module `json.Json` was not found in this Roc project.

You're attempting to use this module here:
**can_import_json.md:1:1:1:17:**
```roc
import json.Json
```
^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
KwImport(1:1-1:7),LowerIdent(1:8-1:12),NoSpaceDotUpperIdent(1:12-1:17),
LowerIdent(3:1-3:5),OpAssign(3:6-3:7),UpperIdent(3:8-3:12),NoSpaceDotLowerIdent(3:12-3:17),
EndOfFile(4:1-4:1),
~~~
# PARSE
~~~clojure
(file @1.1-3.17
	(type-module @1.1-1.7)
	(statements
		(s-import @1.1-1.17 (raw "json.Json"))
		(s-decl @3.1-3.17
			(p-ident @3.1-3.5 (raw "main"))
			(e-ident @3.8-3.17 (raw "Json.utf8")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @3.1-3.5 (ident "main"))
		(e-lookup-external @3.8-3.17
			(module-idx "2")
			(target-node-idx "0")))
	(s-import @1.1-1.17 (module "json.Json") (qualifier "json")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.5 (type "Error")))
	(expressions
		(expr @3.8-3.17 (type "Error"))))
~~~

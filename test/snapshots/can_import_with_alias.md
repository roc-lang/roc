# META
~~~ini
description=Import with explicit alias
type=snippet
~~~
# SOURCE
~~~roc
import json.Json as MyJson

main = MyJson.decode
~~~
# EXPECTED
MODULE NOT FOUND - can_import_with_alias.md:1:1:1:27
# PROBLEMS
**MODULE NOT FOUND**
The module `json.Json` was not found in this Roc project.

You're attempting to use this module here:
**can_import_with_alias.md:1:1:1:27:**
```roc
import json.Json as MyJson
```
^^^^^^^^^^^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
KwImport(1:1-1:7),LowerIdent(1:8-1:12),NoSpaceDotUpperIdent(1:12-1:17),KwAs(1:18-1:20),UpperIdent(1:21-1:27),
LowerIdent(3:1-3:5),OpAssign(3:6-3:7),UpperIdent(3:8-3:14),NoSpaceDotLowerIdent(3:14-3:21),
EndOfFile(4:1-4:1),
~~~
# PARSE
~~~clojure
(file @1.1-3.21
	(type-module @1.1-1.7)
	(statements
		(s-import @1.1-1.27 (raw "json.Json") (alias "MyJson"))
		(s-decl @3.1-3.21
			(p-ident @3.1-3.5 (raw "main"))
			(e-ident @3.8-3.21 (raw "MyJson.decode")))))
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
		(e-lookup-external @3.8-3.21
			(module-idx "0")
			(target-node-idx "0")))
	(s-import @1.1-1.27 (module "json.Json") (qualifier "json") (alias "MyJson")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.5 (type "Error")))
	(expressions
		(expr @3.8-3.21 (type "Error"))))
~~~

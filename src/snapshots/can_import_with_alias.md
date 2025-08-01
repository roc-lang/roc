# META
~~~ini
description=Import with explicit alias
type=file
~~~
# SOURCE
~~~roc
module []

import json.Json as MyJson

main = MyJson.decode
~~~
# EXPECTED
MODULE NOT FOUND - can_import_with_alias.md:3:1:3:27
# PROBLEMS
**MODULE NOT FOUND**
The module `Json.Json` was not found in this Roc project.

You're attempting to use this module here:
**can_import_with_alias.md:3:1:3:27:**
```roc
import json.Json as MyJson
```
^^^^^^^^^^^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
KwImport(3:1-3:7),LowerIdent(3:8-3:12),NoSpaceDotUpperIdent(3:12-3:17),KwAs(3:18-3:20),UpperIdent(3:21-3:27),
LowerIdent(5:1-5:5),OpAssign(5:6-5:7),UpperIdent(5:8-5:14),NoSpaceDotLowerIdent(5:14-5:21),EndOfFile(5:21-5:21),
~~~
# PARSE
~~~clojure
(file @1.1-5.21
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-import @3.1-3.27 (raw "json.Json") (alias "MyJson"))
		(s-decl @5.1-5.21
			(p-ident @5.1-5.5 (raw "main"))
			(e-ident @5.8-5.21 (raw "MyJson.decode")))))
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
		(e-lookup-external @5.8-5.21
			(module-idx "0")
			(target-node-idx "0")))
	(s-import @3.1-3.27 (module "json.Json") (qualifier "json") (alias "MyJson")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @5.1-5.5 (type "Error")))
	(expressions
		(expr @5.8-5.21 (type "Error"))))
~~~

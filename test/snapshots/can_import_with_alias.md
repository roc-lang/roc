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
DOES NOT EXIST - can_import_with_alias.md:3:8:3:21
# PROBLEMS
**MODULE NOT FOUND**
The module `json.Json` was not found in this Roc project.

You're attempting to use this module here:
**can_import_with_alias.md:1:1:1:27:**
```roc
import json.Json as MyJson
```
^^^^^^^^^^^^^^^^^^^^^^^^^^


**DOES NOT EXIST**
`MyJson.decode` does not exist.

**can_import_with_alias.md:3:8:3:21:**
```roc
main = MyJson.decode
```
       ^^^^^^^^^^^^^


# TOKENS
~~~zig
KwImport,LowerIdent,NoSpaceDotUpperIdent,KwAs,UpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-import (raw "json.Json") (alias "MyJson"))
		(s-decl
			(p-ident (raw "main"))
			(e-ident (raw "MyJson.decode")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "main"))
		(e-runtime-error (tag "qualified_ident_does_not_exist")))
	(s-import (module "json.Json")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~

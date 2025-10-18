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
UNDEFINED VARIABLE - can_import_json.md:3:8:3:17
# PROBLEMS
**MODULE NOT FOUND**
The module `json.Json` was not found in this Roc project.

You're attempting to use this module here:
**can_import_json.md:1:1:1:17:**
```roc
import json.Json
```
^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `Json.utf8` in this scope.
Is there an `import` or `exposing` missing up-top?

**can_import_json.md:3:8:3:17:**
```roc
main = Json.utf8
```
       ^^^^^^^^^


# TOKENS
~~~zig
KwImport,LowerIdent,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-import (raw "json.Json"))
		(s-decl
			(p-ident (raw "main"))
			(e-ident (raw "Json.utf8")))))
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
		(e-runtime-error (tag "ident_not_in_scope")))
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

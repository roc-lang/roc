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
DUPLICATE DEFINITION - can_import_json.md:1:1:1:17
NAME NOT IN SCOPE - can_import_json.md:3:8:3:17
# PROBLEMS

┌──────────────────────┐
│ DUPLICATE DEFINITION ├─ The name `Json` is being redeclared here. ──────────┐
└┬─────────────────────┘                                                      │
 │                                                                            │
 │  import json.Json                                                          │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                          │
 └──────────────────────────────────────────────────── can_import_json.md:1:1 ┘

    In this scope, `Json` was already defined here:
      ┌───────────────────────────────────────────────────────────────────────┐
    1 │  import json.Json                                                     │
      │  ‾                                                                    │
      └─────────────────────────────────────────────── can_import_json.md:1:1 ┘


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `utf8` in this scope. ────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  main = Json.utf8                                                          │
 │         ‾‾‾‾‾‾‾‾‾                                                          │
 └──────────────────────────────────────────────────── can_import_json.md:3:8 ┘

    Is it misspelled, or is there an import missing?

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

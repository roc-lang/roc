# META
~~~ini
description=Import with mod-qualified usage
type=snippet
~~~
# SOURCE
~~~roc
import json.Json

main = Json.utf8
~~~
# EXPECTED
DUPLICATE DEFINITION - can_import_json.md:1:1:1:17
DOES NOT EXIST - can_import_json.md:3:13:3:17
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


┌────────────────┐
│ DOES NOT EXIST ├─ `utf8` was not found in `Json`. ──────────────────────────┐
└┬───────────────┘                                                            │
 │                                                                            │
 │  main = Json.utf8                                                          │
 │              ‾‾‾‾                                                          │
 └─────────────────────────────────────────────────── can_import_json.md:3:13 ┘

    Check that `utf8` is spelled correctly and that `Json` exposes it.

# TOKENS
~~~zig
KwImport,LowerIdent,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
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
		(e-runtime-error (tag "qualified_ident_does_not_exist")))
	(s-import (mod "json.Json")
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

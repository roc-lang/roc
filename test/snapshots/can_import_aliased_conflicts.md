# META
~~~ini
description=Import alias name conflicts
type=snippet
~~~
# SOURCE
~~~roc
import json.Json as MyModule
import http.Client as MyModule

main = {
    x = MyModule.parse
    x
}
~~~
# EXPECTED
DUPLICATE DEFINITION - can_import_aliased_conflicts.md:2:1:2:31
NAME NOT IN SCOPE - can_import_aliased_conflicts.md:5:9:5:23
# PROBLEMS

┌──────────────────────┐
│ DUPLICATE DEFINITION ├─ The name `MyModule` is being redeclared here. ──────┐
└┬─────────────────────┘                                                      │
 │                                                                            │
 │  import http.Client as MyModule                                            │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                            │
 └─────────────────────────────────────── can_import_aliased_conflicts.md:2:1 ┘

    In this scope, `MyModule` was already defined here:
      ┌───────────────────────────────────────────────────────────────────────┐
    1 │  import json.Json as MyModule                                         │
      │  ‾                                                                    │
      └────────────────────────────────── can_import_aliased_conflicts.md:1:1 ┘


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `parse` in this scope. ───────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  x = MyModule.parse                                                        │
 │      ‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                        │
 └─────────────────────────────────────── can_import_aliased_conflicts.md:5:9 ┘

    Is it misspelled, or is there an import missing?

# TOKENS
~~~zig
KwImport,LowerIdent,NoSpaceDotUpperIdent,KwAs,UpperIdent,
KwImport,LowerIdent,NoSpaceDotUpperIdent,KwAs,UpperIdent,
LowerIdent,OpAssign,OpenCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-import (raw "json.Json") (alias "MyModule"))
		(s-import (raw "http.Client") (alias "MyModule"))
		(s-decl
			(p-ident (raw "main"))
			(e-block
				(statements
					(s-decl
						(p-ident (raw "x"))
						(e-ident (raw "MyModule.parse")))
					(e-ident (raw "x")))))))
~~~
# FORMATTED
~~~roc
import json.Json as MyModule
import http.Client as MyModule

main = {
	x = MyModule.parse
	x
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "main"))
		(e-block
			(s-let
				(p-assign (ident "x"))
				(e-runtime-error (tag "ident_not_in_scope")))
			(e-lookup-local
				(p-assign (ident "x")))))
	(s-import (module "json.Json")
		(exposes))
	(s-import (module "http.Client")
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

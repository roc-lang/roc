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
NAME NOT IN SCOPE - can_import_with_alias.md:3:8:3:21
# PROBLEMS

┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `decode` in this scope. ──────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  main = MyJson.decode                                                      │
 │         ‾‾‾‾‾‾‾‾‾‾‾‾‾                                                      │
 └────────────────────────────────────────────── can_import_with_alias.md:3:8 ┘

    Is it misspelled, or is there an import missing?

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

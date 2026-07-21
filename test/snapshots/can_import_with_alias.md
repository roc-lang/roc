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
DOES NOT EXIST - can_import_with_alias.md:3:15:3:21
# PROBLEMS

┌────────────────┐
│ DOES NOT EXIST ├─ `decode` was not found in `MyJson`. ──────────────────────┐
└┬───────────────┘                                                            │
 │                                                                            │
 │  main = MyJson.decode                                                      │
 │                ‾‾‾‾‾‾                                                      │
 └───────────────────────────────────────────── can_import_with_alias.md:3:15 ┘

    Check that `decode` is spelled correctly and that `MyJson` exposes it.

# TOKENS
~~~zig
KwImport,LowerIdent,NoSpaceDotUpperIdent,KwAs,UpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
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

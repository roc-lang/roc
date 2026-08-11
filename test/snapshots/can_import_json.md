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
NAME NOT IN SCOPE - can_import_json.md:3:8:3:17
# PROBLEMS
── ● duplicate definition ─────────────────────────────── can_import_json.md:1:1

The name Json is being redeclared here:

import json.Json
^^^^^^^^^^^^^^^^

In this scope, Json was already defined in can_import_json.md:1:1:

import json.Json
^

── ✗ name not in scope ────────────────────────────────── can_import_json.md:3:8

Nothing is named utf8 in this scope.

main = Json.utf8
       ^^^^^^^^^

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
		(e-runtime-error (tag "ident_not_in_scope")))
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

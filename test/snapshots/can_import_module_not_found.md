# META
~~~ini
description=Import of non-existent module
type=snippet
~~~
# SOURCE
~~~roc
import nonexistent.Module

main = Module.something
~~~
# EXPECTED
NAME NOT IN SCOPE - can_import_module_not_found.md:3:8:3:24
# PROBLEMS

┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `something` in this scope. ───────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  main = Module.something                                                   │
 │         ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                   │
 └──────────────────────────────────────── can_import_module_not_found.md:3:8 ┘

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
		(s-import (raw "nonexistent.Module"))
		(s-decl
			(p-ident (raw "main"))
			(e-ident (raw "Module.something")))))
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
	(s-import (module "nonexistent.Module")
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

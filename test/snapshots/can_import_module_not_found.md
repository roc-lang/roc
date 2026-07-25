# META
~~~ini
description=Import of non-existent mod
type=snippet
~~~
# SOURCE
~~~roc
import nonexistent.Mod

main = Mod.something
~~~
# EXPECTED
DOES NOT EXIST - can_import_mod_not_found.md:3:12:3:21
# PROBLEMS

┌────────────────┐
│ DOES NOT EXIST ├─ `something` was not found in `Mod`. ──────────────────────┐
└┬───────────────┘                                                            │
 │                                                                            │
 │  main = Mod.something                                                      │
 │             ‾‾‾‾‾‾‾‾‾                                                      │
 └─────────────────────────────────────── can_import_mod_not_found.md:3:12 ┘

    Check that `something` is spelled correctly and that `Mod` exposes it.

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
		(s-import (raw "nonexistent.Mod"))
		(s-decl
			(p-ident (raw "main"))
			(e-ident (raw "Mod.something")))))
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
	(s-import (mod "nonexistent.Mod")
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

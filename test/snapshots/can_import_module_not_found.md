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
NAME NOT IN SCOPE - can_import_mod_not_found.md:3:8:3:21
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 3 8) (end 3 21))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "something")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "can_import_mod_not_found.md") (start 3 8) (end 3 21) (annotation error) (line-text "main = Mod.something")))))
~~~
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
		(e-runtime-error (tag "ident_not_in_scope")))
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

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
~~~clojure
(reports
	(report
		(severity warning)
		(title "Duplicate Definition")
		(region (start 1 1) (end 1 17))
		(headline
			(reflow "The name ")
			(annotated symbol-unqualified "Json")
			(reflow " is being redeclared here:"))
		(document
			(source-region (file "can_import_json.md") (start 1 1) (end 1 17) (annotation error) (line-text "import json.Json"))
			(line-break)
			(reflow "In this scope, ")
			(annotated symbol-unqualified "Json")
			(reflow " was already defined in ")
			(source-location
				(file "can_import_json.md")
				(line 1)
				(column 1))
			(reflow ":")
			(line-break)
			(source-region (file "can_import_json.md") (start 1 1) (end 1 1) (annotation dim) (line-text "import json.Json"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 3 8) (end 3 17))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "utf8")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "can_import_json.md") (start 3 8) (end 3 17) (annotation error) (line-text "main = Json.utf8")))))
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

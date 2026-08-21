# META
~~~ini
description=Import alias name conflicts
type=snippet
~~~
# SOURCE
~~~roc
import json.Json as MyMod
import http.Client as MyMod

main = {
    x = MyMod.parse
    x
}
~~~
# EXPECTED
DUPLICATE DEFINITION - can_import_aliased_conflicts.md:2:1:2:28
NAME NOT IN SCOPE - can_import_aliased_conflicts.md:5:9:5:20
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Duplicate Definition")
		(region (start 2 1) (end 2 28))
		(headline
			(reflow "The name ")
			(annotated symbol-unqualified "MyMod")
			(reflow " is being redeclared here:"))
		(document
			(source-region (file "can_import_aliased_conflicts.md") (start 2 1) (end 2 28) (annotation error) (line-text "import http.Client as MyMod"))
			(line-break)
			(reflow "In this scope, ")
			(annotated symbol-unqualified "MyMod")
			(reflow " was already defined in ")
			(source-location
				(file "can_import_aliased_conflicts.md")
				(line 1)
				(column 1))
			(reflow ":")
			(line-break)
			(source-region (file "can_import_aliased_conflicts.md") (start 1 1) (end 1 1) (annotation dim) (line-text "import json.Json as MyMod"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 5 9) (end 5 20))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "parse")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "can_import_aliased_conflicts.md") (start 5 9) (end 5 20) (annotation error) (line-text "    x = MyMod.parse")))))
~~~
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
	(type-mod)
	(statements
		(s-import (raw "json.Json") (alias "MyMod"))
		(s-import (raw "http.Client") (alias "MyMod"))
		(s-decl
			(p-ident (raw "main"))
			(e-block
				(statements
					(s-decl
						(p-ident (raw "x"))
						(e-ident (raw "MyMod.parse")))
					(e-ident (raw "x")))))))
~~~
# FORMATTED
~~~roc
import json.Json as MyMod
import http.Client as MyMod

main = {
	x = MyMod.parse
	x
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "main"))
		(e-runtime-error (tag "erroneous_value_expr")))
	(s-import (mod "json.Json")
		(exposes))
	(s-import (mod "http.Client")
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

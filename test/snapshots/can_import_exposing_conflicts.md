# META
~~~ini
description=Exposed item name conflicts with local definitions
type=snippet
~~~
# SOURCE
~~~roc
import json.Json exposing [parse]

# Local definition with same name as exposed item
parse = 42

main = {
    result = parse
    result
}
~~~
# EXPECTED
DUPLICATE DEFINITION - can_import_exposing_conflicts.md:1:1:1:34
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Duplicate Definition")
		(region (start 1 1) (end 1 34))
		(headline
			(reflow "The name ")
			(annotated symbol-unqualified "Json")
			(reflow " is being redeclared here:"))
		(document
			(source-region (file "can_import_exposing_conflicts.md") (start 1 1) (end 1 34) (annotation error) (line-text "import json.Json exposing [parse]"))
			(line-break)
			(reflow "In this scope, ")
			(annotated symbol-unqualified "Json")
			(reflow " was already defined in ")
			(source-location
				(file "can_import_exposing_conflicts.md")
				(line 1)
				(column 1))
			(reflow ":")
			(line-break)
			(source-region (file "can_import_exposing_conflicts.md") (start 1 1) (end 1 1) (annotation dim) (line-text "import json.Json exposing [parse]")))))
~~~
# TOKENS
~~~zig
KwImport,LowerIdent,NoSpaceDotUpperIdent,KwExposing,OpenSquare,LowerIdent,CloseSquare,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,OpenCurly,
LowerIdent,OpAssign,LowerIdent,
LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-import (raw "json.Json")
			(exposing
				(exposed-lower-ident
					(text "parse"))))
		(s-decl
			(p-ident (raw "parse"))
			(e-int (raw "42")))
		(s-decl
			(p-ident (raw "main"))
			(e-block
				(statements
					(s-decl
						(p-ident (raw "result"))
						(e-ident (raw "parse")))
					(e-ident (raw "result")))))))
~~~
# FORMATTED
~~~roc
import json.Json exposing [parse]

# Local definition with same name as exposed item
parse = 42

main = {
	result = parse
	result
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "parse"))
		(e-num (value "42")))
	(d-let
		(p-assign (ident "main"))
		(e-block
			(s-let
				(p-assign (ident "result"))
				(e-lookup-local
					(p-assign (ident "parse"))))
			(e-lookup-local
				(p-assign (ident "result")))))
	(s-import (mod "json.Json")
		(exposes
			(exposed (name "parse") (wildcard false)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Dec"))
		(patt (type "Dec")))
	(expressions
		(expr (type "Dec"))
		(expr (type "Dec"))))
~~~

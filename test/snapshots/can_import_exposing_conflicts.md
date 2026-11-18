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
RESERVED TYPE NAME - can_import_exposing_conflicts.md:1:12:1:17
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Reserved Type Name")
		(region (start 1 12) (end 1 17))
		(headline
			(text "This import cannot bind ")
			(annotated symbol-unqualified "Json")
			(text " because that name is reserved for a builtin type."))
		(document
			(reflow "Use `as` to give the imported type a different name.")
			(line-break)
			(source-region (file "can_import_exposing_conflicts.md") (start 1 12) (end 1 17) (annotation error) (line-text "import json.Json exposing [parse]")))))
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

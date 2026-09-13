# META
~~~ini
description=External declaration lookup from json mod
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

import pf.Stdout
import json.Json

main! = |_| {
    # This should create an external declaration for json.Json.utf8
    result = Json.utf8("Hello from external mod!")
    Stdout.line!(result)
}
~~~
# EXPECTED
DUPLICATE DEFINITION - external_decl_lookup.md:4:1:4:17
NAME NOT IN SCOPE - external_decl_lookup.md:8:14:8:23
NAME NOT IN SCOPE - external_decl_lookup.md:9:5:9:17
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Duplicate Definition")
		(region (start 4 1) (end 4 17))
		(headline
			(reflow "The name ")
			(annotated symbol-unqualified "Json")
			(reflow " is being redeclared here:"))
		(document
			(source-region (file "external_decl_lookup.md") (start 4 1) (end 4 17) (annotation error) (line-text "import json.Json"))
			(line-break)
			(reflow "In this scope, ")
			(annotated symbol-unqualified "Json")
			(reflow " was already defined in ")
			(source-location
				(file "external_decl_lookup.md")
				(line 1)
				(column 1))
			(reflow ":")
			(line-break)
			(source-region (file "external_decl_lookup.md") (start 1 1) (end 1 1) (annotation dim) (line-text "app [main!] { pf: platform \"../basic-cli/platform.roc\" }"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 8 14) (end 8 23))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "utf8")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "external_decl_lookup.md") (start 8 14) (end 8 23) (annotation error) (line-text "    result = Json.utf8(\"Hello from external mod!\")"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 9 5) (end 9 17))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "line!")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "external_decl_lookup.md") (start 9 5) (end 9 17) (annotation error) (line-text "    Stdout.line!(result)")))))
~~~
# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
KwImport,LowerIdent,NoSpaceDotUpperIdent,
KwImport,LowerIdent,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(app
		(provides
			(exposed-lower-ident
				(text "main!")))
		(record-field (name "pf")
			(e-string
				(e-string-part (raw "../basic-cli/platform.roc"))))
		(packages
			(record-field (name "pf")
				(e-string
					(e-string-part (raw "../basic-cli/platform.roc"))))))
	(statements
		(s-import (raw "pf.Stdout"))
		(s-import (raw "json.Json"))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "result"))
							(e-apply
								(e-ident (raw "Json.utf8"))
								(e-string
									(e-string-part (raw "Hello from external mod!")))))
						(e-apply
							(e-ident (raw "Stdout.line!"))
							(e-ident (raw "result")))))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

import pf.Stdout
import json.Json

main! = |_| {
	# This should create an external declaration for json.Json.utf8
	result = Json.utf8("Hello from external mod!")
	Stdout.line!(result)
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "main!"))
		(e-runtime-error (tag "erroneous_value_expr")))
	(s-import (mod "pf.Stdout")
		(exposes))
	(s-import (mod "json.Json")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_arg -> Error")))
	(expressions
		(expr (type "_arg -> Error"))))
~~~

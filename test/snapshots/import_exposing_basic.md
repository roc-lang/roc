# META
~~~ini
description=Import with exposing clause and usage of exposed items
type=snippet
~~~
# SOURCE
~~~roc
import json.Json exposing [decode, to_str]

main = {
    data = { name: "Alice", age: 30 }
    encoded = to_str(data)
    decoded = decode(encoded)
    decoded
}
~~~
# EXPECTED
DUPLICATE DEFINITION - import_exposing_basic.md:1:1:1:43
NAME NOT IN SCOPE - import_exposing_basic.md:5:15:5:21
NAME NOT IN SCOPE - import_exposing_basic.md:6:15:6:21
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Duplicate Definition")
		(region (start 1 1) (end 1 43))
		(headline
			(reflow "The name ")
			(annotated symbol-unqualified "Json")
			(reflow " is being redeclared here:"))
		(document
			(source-region (file "import_exposing_basic.md") (start 1 1) (end 1 43) (annotation error) (line-text "import json.Json exposing [decode, to_str]"))
			(line-break)
			(reflow "In this scope, ")
			(annotated symbol-unqualified "Json")
			(reflow " was already defined in ")
			(source-location
				(file "import_exposing_basic.md")
				(line 1)
				(column 1))
			(reflow ":")
			(line-break)
			(source-region (file "import_exposing_basic.md") (start 1 1) (end 1 1) (annotation dim) (line-text "import json.Json exposing [decode, to_str]"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 5 15) (end 5 21))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "to_str")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "import_exposing_basic.md") (start 5 15) (end 5 21) (annotation error) (line-text "    encoded = to_str(data)"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 6 15) (end 6 21))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "decode")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "import_exposing_basic.md") (start 6 15) (end 6 21) (annotation error) (line-text "    decoded = decode(encoded)")))))
~~~
# TOKENS
~~~zig
KwImport,LowerIdent,NoSpaceDotUpperIdent,KwExposing,OpenSquare,LowerIdent,Comma,LowerIdent,CloseSquare,
LowerIdent,OpAssign,OpenCurly,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,Int,CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
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
					(text "decode"))
				(exposed-lower-ident
					(text "to_str"))))
		(s-decl
			(p-ident (raw "main"))
			(e-block
				(statements
					(s-decl
						(p-ident (raw "data"))
						(e-record
							(field (field "name")
								(e-string
									(e-string-part (raw "Alice"))))
							(field (field "age")
								(e-int (raw "30")))))
					(s-decl
						(p-ident (raw "encoded"))
						(e-apply
							(e-ident (raw "to_str"))
							(e-ident (raw "data"))))
					(s-decl
						(p-ident (raw "decoded"))
						(e-apply
							(e-ident (raw "decode"))
							(e-ident (raw "encoded"))))
					(e-ident (raw "decoded")))))))
~~~
# FORMATTED
~~~roc
import json.Json exposing [decode, to_str]

main = {
	data = { name: "Alice", age: 30 }
	encoded = to_str(data)
	decoded = decode(encoded)
	decoded
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "main"))
		(e-runtime-error (tag "erroneous_value_expr")))
	(s-import (mod "json.Json")
		(exposes
			(exposed (name "decode") (wildcard false))
			(exposed (name "to_str") (wildcard false)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~

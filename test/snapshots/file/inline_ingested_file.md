# META
~~~ini
description=inline_ingested_file
type=snippet
~~~
# SOURCE
~~~roc
import "users.json" as data : Str
import Json

foo = Json.parse(data)
~~~
# EXPECTED
DUPLICATE DEFINITION - inline_ingested_file.md:2:1:2:12
FILE NOT FOUND - inline_ingested_file.md:1:1:1:34
MOD NOT FOUND - inline_ingested_file.md:2:1:2:12
TYPE NOT DETERMINED - inline_ingested_file.md:4:7:4:17
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Duplicate Definition")
		(region (start 2 1) (end 2 12))
		(headline
			(reflow "The name ")
			(annotated symbol-unqualified "Json")
			(reflow " is being redeclared here:"))
		(document
			(source-region (file "inline_ingested_file.md") (start 2 1) (end 2 12) (annotation error) (line-text "import Json"))
			(line-break)
			(reflow "In this scope, ")
			(annotated symbol-unqualified "Json")
			(reflow " was already defined in ")
			(source-location
				(file "inline_ingested_file.md")
				(line 1)
				(column 1))
			(reflow ":")
			(line-break)
			(source-region (file "inline_ingested_file.md") (start 1 1) (end 1 1) (annotation dim) (line-text "import \"users.json\" as data : Str"))))
	(report
		(severity runtime_error)
		(title "File Not Found")
		(region (start 1 1) (end 1 34))
		(headline
			(reflow "The file ")
			(annotated mod "users.json")
			(reflow " was not found."))
		(document
			(reflow "Make sure the file exists relative to your source file.")
			(line-break)
			(source-region (file "inline_ingested_file.md") (start 1 1) (end 1 34) (annotation error) (line-text "import \"users.json\" as data : Str"))))
	(report
		(severity runtime_error)
		(title "Mod Not Found")
		(region (start 2 1) (end 2 12))
		(headline
			(text "The mod ")
			(annotated code "Json")
			(reflow " was not found in this Roc project."))
		(document
			(source-region (file "inline_ingested_file.md") (start 2 1) (end 2 12) (annotation error) (line-text "import Json"))))
	(report
		(severity runtime_error)
		(title "Type Not Determined")
		(region (start 4 7) (end 4 17))
		(headline
			(reflow "Nothing in this program determines the type this")
			(reflow " ")
			(annotated code "parser_for")
			(reflow " ")
			(reflow "method is called on:"))
		(document
			(source-region (file "inline_ingested_file.md") (start 4 7) (end 4 17) (annotation error) (line-text "foo = Json.parse(data)"))
			(line-break)
			(reflow "Without knowing which type it is, there's no way to tell which")
			(reflow " ")
			(annotated code "parser_for")
			(reflow " ")
			(reflow "method to use.")
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "Add a type annotation saying which type it should be."))))
~~~
# TOKENS
~~~zig
KwImport,StringStart,StringPart,StringEnd,KwAs,LowerIdent,OpColon,UpperIdent,
KwImport,UpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-file-import
			(path "users.json")
			(name "data")
			(type "Str"))
		(s-import (raw "Json"))
		(s-decl
			(p-ident (raw "foo"))
			(e-apply
				(e-ident (raw "Json.parse"))
				(e-ident (raw "data"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "data"))
		(e-runtime-error (tag "file_import_not_found")))
	(d-let
		(p-assign (ident "foo"))
		(e-runtime-error (tag "erroneous_value_expr")
			(e-runtime-error (tag "erroneous_value_expr"))
			(e-runtime-error (tag "erroneous_value_expr"))))
	(s-import (mod "Json")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error"))
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))
		(expr (type "Error"))))
~~~

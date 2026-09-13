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
FILE NOT FOUND - inline_ingested_file.md:1:1:1:34
DUPLICATE DEFINITION - inline_ingested_file.md:2:1:2:12
MISSING METHOD - inline_ingested_file.md:4:7:4:17
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "File Not Found")
		(region (start 1 1) (end 1 34))
		(headline
			(reflow "The file ")
			(annotated mod "users.json")
			(reflow " was not found."))
		(document
			(reflow "Make sure the file exists relative to your source file:")
			(line-break)
			(source-region (file "inline_ingested_file.md") (start 1 1) (end 1 34) (annotation error) (line-text "import \"users.json\" as data : Str"))))
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
		(title "Missing Method")
		(region (start 4 7) (end 4 17))
		(headline
			(reflow "This is trying to dispatch a method named")
			(reflow " ")
			(annotated code "parser_for")
			(reflow " ")
			(reflow "on an unresolved type variable, but unresolved type variables have no methods."))
		(document
			(source-region (file "inline_ingested_file.md") (start 4 7) (end 4 17) (annotation error) (line-text "foo = Json.parse(data)"))
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "You can replace this static dispatch call with an ordinary function call, or force the type variable to become more concrete—for example, by adding a type annotation that narrows its type to something that actually has methods."))))
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
		(e-runtime-error (tag "erroneous_value_expr")))
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

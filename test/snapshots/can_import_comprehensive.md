# META
~~~ini
description=Comprehensive import test with various mod access patterns
type=snippet
~~~
# SOURCE
~~~roc
import json.Json
import http.Client as Http exposing [get, post]
import utils.String as Str

main = {
    client = Http.get
    parser = Json.utf8
    helper = Str.trim

    # Test direct mod access
    result1 = Json.parse

    # Test aliased mod access
    result2 = Http.post

    # Test exposed items (should work without mod prefix)
    result3 = get
    result4 = post

    # Test multiple qualified access
    combined = Str.concat

    (
        client,
        parser,
        helper,
        result1,
        result2,
        result3,
        result4,
        combined,
    )
}
~~~
# EXPECTED
DUPLICATE DEFINITION - can_import_comprehensive.md:1:1:1:17
DUPLICATE DEFINITION - can_import_comprehensive.md:3:1:3:27
NAME NOT IN SCOPE - can_import_comprehensive.md:6:14:6:22
NAME NOT IN SCOPE - can_import_comprehensive.md:7:14:7:23
NAME NOT IN SCOPE - can_import_comprehensive.md:8:14:8:22
NAME NOT IN SCOPE - can_import_comprehensive.md:11:15:11:25
NAME NOT IN SCOPE - can_import_comprehensive.md:14:15:14:24
NAME NOT IN SCOPE - can_import_comprehensive.md:17:15:17:18
NAME NOT IN SCOPE - can_import_comprehensive.md:18:15:18:19
NAME NOT IN SCOPE - can_import_comprehensive.md:21:16:21:26
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
			(source-region (file "can_import_comprehensive.md") (start 1 1) (end 1 17) (annotation error) (line-text "import json.Json"))
			(line-break)
			(reflow "In this scope, ")
			(annotated symbol-unqualified "Json")
			(reflow " was already defined in ")
			(source-location
				(file "can_import_comprehensive.md")
				(line 1)
				(column 1))
			(reflow ":")
			(line-break)
			(source-region (file "can_import_comprehensive.md") (start 1 1) (end 1 1) (annotation dim) (line-text "import json.Json"))))
	(report
		(severity warning)
		(title "Duplicate Definition")
		(region (start 3 1) (end 3 27))
		(headline
			(reflow "The name ")
			(annotated symbol-unqualified "Str")
			(reflow " is being redeclared here:"))
		(document
			(source-region (file "can_import_comprehensive.md") (start 3 1) (end 3 27) (annotation error) (line-text "import utils.String as Str"))
			(line-break)
			(reflow "In this scope, ")
			(annotated symbol-unqualified "Str")
			(reflow " was already defined in ")
			(source-location
				(file "can_import_comprehensive.md")
				(line 1)
				(column 1))
			(reflow ":")
			(line-break)
			(source-region (file "can_import_comprehensive.md") (start 1 1) (end 1 1) (annotation dim) (line-text "import json.Json"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 6 14) (end 6 22))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "get")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "can_import_comprehensive.md") (start 6 14) (end 6 22) (annotation error) (line-text "    client = Http.get"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 7 14) (end 7 23))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "utf8")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "can_import_comprehensive.md") (start 7 14) (end 7 23) (annotation error) (line-text "    parser = Json.utf8"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 8 14) (end 8 22))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "trim")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "can_import_comprehensive.md") (start 8 14) (end 8 22) (annotation error) (line-text "    helper = Str.trim"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 11 15) (end 11 25))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "parse")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "can_import_comprehensive.md") (start 11 15) (end 11 25) (annotation error) (line-text "    result1 = Json.parse"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 14 15) (end 14 24))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "post")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "can_import_comprehensive.md") (start 14 15) (end 14 24) (annotation error) (line-text "    result2 = Http.post"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 17 15) (end 17 18))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "get")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "can_import_comprehensive.md") (start 17 15) (end 17 18) (annotation error) (line-text "    result3 = get"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 18 15) (end 18 19))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "post")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "can_import_comprehensive.md") (start 18 15) (end 18 19) (annotation error) (line-text "    result4 = post"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 21 16) (end 21 26))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "concat")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "can_import_comprehensive.md") (start 21 16) (end 21 26) (annotation error) (line-text "    combined = Str.concat")))))
~~~
# TOKENS
~~~zig
KwImport,LowerIdent,NoSpaceDotUpperIdent,
KwImport,LowerIdent,NoSpaceDotUpperIdent,KwAs,UpperIdent,KwExposing,OpenSquare,LowerIdent,Comma,LowerIdent,CloseSquare,
KwImport,LowerIdent,NoSpaceDotUpperIdent,KwAs,UpperIdent,
LowerIdent,OpAssign,OpenCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,LowerIdent,
LowerIdent,OpAssign,LowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
OpenRound,
LowerIdent,Comma,
LowerIdent,Comma,
LowerIdent,Comma,
LowerIdent,Comma,
LowerIdent,Comma,
LowerIdent,Comma,
LowerIdent,Comma,
LowerIdent,Comma,
CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-import (raw "json.Json"))
		(s-import (raw "http.Client") (alias "Http")
			(exposing
				(exposed-lower-ident
					(text "get"))
				(exposed-lower-ident
					(text "post"))))
		(s-import (raw "utils.String") (alias "Str"))
		(s-decl
			(p-ident (raw "main"))
			(e-block
				(statements
					(s-decl
						(p-ident (raw "client"))
						(e-ident (raw "Http.get")))
					(s-decl
						(p-ident (raw "parser"))
						(e-ident (raw "Json.utf8")))
					(s-decl
						(p-ident (raw "helper"))
						(e-ident (raw "Str.trim")))
					(s-decl
						(p-ident (raw "result1"))
						(e-ident (raw "Json.parse")))
					(s-decl
						(p-ident (raw "result2"))
						(e-ident (raw "Http.post")))
					(s-decl
						(p-ident (raw "result3"))
						(e-ident (raw "get")))
					(s-decl
						(p-ident (raw "result4"))
						(e-ident (raw "post")))
					(s-decl
						(p-ident (raw "combined"))
						(e-ident (raw "Str.concat")))
					(e-tuple
						(e-ident (raw "client"))
						(e-ident (raw "parser"))
						(e-ident (raw "helper"))
						(e-ident (raw "result1"))
						(e-ident (raw "result2"))
						(e-ident (raw "result3"))
						(e-ident (raw "result4"))
						(e-ident (raw "combined"))))))))
~~~
# FORMATTED
~~~roc
import json.Json
import http.Client as Http exposing [get, post]
import utils.String as Str

main = {
	client = Http.get
	parser = Json.utf8
	helper = Str.trim

	# Test direct mod access
	result1 = Json.parse

	# Test aliased mod access
	result2 = Http.post

	# Test exposed items (should work without mod prefix)
	result3 = get
	result4 = post

	# Test multiple qualified access
	combined = Str.concat

	(
		client,
		parser,
		helper,
		result1,
		result2,
		result3,
		result4,
		combined,
	)
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
		(exposes
			(exposed (name "get") (wildcard false))
			(exposed (name "post") (wildcard false))))
	(s-import (mod "utils.String")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "(Error, Error, Error, Error, Error, Error, Error, Error)")))
	(expressions
		(expr (type "(Error, Error, Error, Error, Error, Error, Error, Error)"))))
~~~

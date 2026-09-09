# META
~~~ini
description=Error handling for unresolved qualified names
type=snippet
~~~
# SOURCE
~~~roc
import json.Json
import http.Client as Http

# Test unresolved qualified value
main = Json.NonExistent.method

# Test unresolved qualified type in annotation
parseData : Json.InvalidType -> Str
parseData = |data| Json.stringify(data)

# Test unresolved nested qualification
processRequest : Http.Server.Request -> Http.Server.Response
processRequest = |req| Http.Server.defaultResponse

# Test typo in qualified name
result = Json.prase("test")

# Test unknown mod qualification
config = Unknown.Mod.config

# Test valid mod but invalid member
client = Http.invalidMethod

# Test deeply nested invalid qualification
parser = Json.Parser.Advanced.NonExistent.create
~~~
# EXPECTED
DUPLICATE DEFINITION - can_import_unresolved_qualified.md:1:1:1:17
NAME NOT IN SCOPE - can_import_unresolved_qualified.md:5:8:5:31
MOD NOT FOUND - can_import_unresolved_qualified.md:8:17:8:29
NAME NOT IN SCOPE - can_import_unresolved_qualified.md:9:20:9:34
MOD NOT FOUND - can_import_unresolved_qualified.md:12:29:12:37
MOD NOT FOUND - can_import_unresolved_qualified.md:12:52:12:61
NAME NOT IN SCOPE - can_import_unresolved_qualified.md:13:24:13:51
UNUSED VARIABLE - can_import_unresolved_qualified.md:13:19:13:22
NAME NOT IN SCOPE - can_import_unresolved_qualified.md:16:10:16:20
DOES NOT EXIST - can_import_unresolved_qualified.md:19:10:19:28
NAME NOT IN SCOPE - can_import_unresolved_qualified.md:22:10:22:28
NAME NOT IN SCOPE - can_import_unresolved_qualified.md:25:10:25:49
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
			(source-region (file "can_import_unresolved_qualified.md") (start 1 1) (end 1 17) (annotation error) (line-text "import json.Json"))
			(line-break)
			(reflow "In this scope, ")
			(annotated symbol-unqualified "Json")
			(reflow " was already defined in ")
			(source-location
				(file "can_import_unresolved_qualified.md")
				(line 1)
				(column 1))
			(reflow ":")
			(line-break)
			(source-region (file "can_import_unresolved_qualified.md") (start 1 1) (end 1 1) (annotation dim) (line-text "import json.Json"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 5 8) (end 5 31))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "method")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "can_import_unresolved_qualified.md") (start 5 8) (end 5 31) (annotation error) (line-text "main = Json.NonExistent.method"))))
	(report
		(severity runtime_error)
		(title "Mod Not Found")
		(region (start 8 17) (end 8 29))
		(headline
			(text "This ")
			(annotated code "InvalidType")
			(reflow " type is declared to be in ")
			(annotated code "json.Json")
			(reflow ", which does not exist."))
		(document
			(source-region (file "can_import_unresolved_qualified.md") (start 8 17) (end 8 29) (annotation error) (line-text "parseData : Json.InvalidType -> Str"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 9 20) (end 9 34))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "stringify")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "can_import_unresolved_qualified.md") (start 9 20) (end 9 34) (annotation error) (line-text "parseData = |data| Json.stringify(data)"))))
	(report
		(severity runtime_error)
		(title "Mod Not Found")
		(region (start 12 29) (end 12 37))
		(headline
			(text "This ")
			(annotated code "Server.Request")
			(reflow " type is declared to be in ")
			(annotated code "http.Client")
			(reflow ", which does not exist."))
		(document
			(source-region (file "can_import_unresolved_qualified.md") (start 12 29) (end 12 37) (annotation error) (line-text "processRequest : Http.Server.Request -> Http.Server.Response"))))
	(report
		(severity runtime_error)
		(title "Mod Not Found")
		(region (start 12 52) (end 12 61))
		(headline
			(text "This ")
			(annotated code "Server.Response")
			(reflow " type is declared to be in ")
			(annotated code "http.Client")
			(reflow ", which does not exist."))
		(document
			(source-region (file "can_import_unresolved_qualified.md") (start 12 52) (end 12 61) (annotation error) (line-text "processRequest : Http.Server.Request -> Http.Server.Response"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 13 24) (end 13 51))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "defaultResponse")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "can_import_unresolved_qualified.md") (start 13 24) (end 13 51) (annotation error) (line-text "processRequest = |req| Http.Server.defaultResponse"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 13 19) (end 13 22))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "req")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_req")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "can_import_unresolved_qualified.md") (start 13 19) (end 13 22) (annotation error) (line-text "processRequest = |req| Http.Server.defaultResponse"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 16 10) (end 16 20))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "prase")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "can_import_unresolved_qualified.md") (start 16 10) (end 16 20) (annotation error) (line-text "result = Json.prase(\"test\")"))))
	(report
		(severity runtime_error)
		(title "Does Not Exist")
		(region (start 19 10) (end 19 28))
		(headline
			(annotated symbol-unqualified "Unknown.Mod.config")
			(reflow " does not exist."))
		(document
			(source-region (file "can_import_unresolved_qualified.md") (start 19 10) (end 19 28) (annotation error) (line-text "config = Unknown.Mod.config"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 22 10) (end 22 28))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "invalidMethod")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "can_import_unresolved_qualified.md") (start 22 10) (end 22 28) (annotation error) (line-text "client = Http.invalidMethod"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 25 10) (end 25 49))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "create")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "can_import_unresolved_qualified.md") (start 25 10) (end 25 49) (annotation error) (line-text "parser = Json.Parser.Advanced.NonExistent.create")))))
~~~
# TOKENS
~~~zig
KwImport,LowerIdent,NoSpaceDotUpperIdent,
KwImport,LowerIdent,NoSpaceDotUpperIdent,KwAs,UpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,OpArrow,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotLowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-import (raw "json.Json"))
		(s-import (raw "http.Client") (alias "Http"))
		(s-decl
			(p-ident (raw "main"))
			(e-ident (raw "Json.NonExistent.method")))
		(s-type-anno (name "parseData")
			(ty-fn
				(ty (name "Json.InvalidType"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "parseData"))
			(e-lambda
				(args
					(p-ident (raw "data")))
				(e-apply
					(e-ident (raw "Json.stringify"))
					(e-ident (raw "data")))))
		(s-type-anno (name "processRequest")
			(ty-fn
				(ty (name "Http.Server.Request"))
				(ty (name "Http.Server.Response"))))
		(s-decl
			(p-ident (raw "processRequest"))
			(e-lambda
				(args
					(p-ident (raw "req")))
				(e-ident (raw "Http.Server.defaultResponse"))))
		(s-decl
			(p-ident (raw "result"))
			(e-apply
				(e-ident (raw "Json.prase"))
				(e-string
					(e-string-part (raw "test")))))
		(s-decl
			(p-ident (raw "config"))
			(e-ident (raw "Unknown.Mod.config")))
		(s-decl
			(p-ident (raw "client"))
			(e-ident (raw "Http.invalidMethod")))
		(s-decl
			(p-ident (raw "parser"))
			(e-ident (raw "Json.Parser.Advanced.NonExistent.create")))))
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
	(d-let
		(p-assign (ident "parseData"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-malformed)
				(ty-lookup (name "Str") (builtin)))))
	(d-let
		(p-assign (ident "processRequest"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-malformed)
				(ty-malformed))))
	(d-let
		(p-assign (ident "result"))
		(e-runtime-error (tag "erroneous_value_expr")))
	(d-let
		(p-assign (ident "config"))
		(e-runtime-error (tag "qualified_ident_does_not_exist")))
	(d-let
		(p-assign (ident "client"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(d-let
		(p-assign (ident "parser"))
		(e-runtime-error (tag "ident_not_in_scope")))
	(s-import (mod "json.Json")
		(exposes))
	(s-import (mod "http.Client")
		(exposes)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error"))
		(patt (type "Error -> Str"))
		(patt (type "Error -> Error"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "Error"))
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))
		(expr (type "Error -> Str"))
		(expr (type "Error -> Error"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Error"))
		(expr (type "Error"))))
~~~

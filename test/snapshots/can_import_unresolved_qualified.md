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
RESERVED TYPE NAME - can_import_unresolved_qualified.md:1:12:1:17
UNUSED VARIABLE - can_import_unresolved_qualified.md:13:19:13:22
DOES NOT EXIST - can_import_unresolved_qualified.md:19:10:19:28
MOD NOT FOUND - can_import_unresolved_qualified.md:12:29:12:37
MOD NOT FOUND - can_import_unresolved_qualified.md:12:52:12:61
DOES NOT EXIST - can_import_unresolved_qualified.md:13:24:13:51
DOES NOT EXIST - can_import_unresolved_qualified.md:22:10:22:28
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
			(source-region (file "can_import_unresolved_qualified.md") (start 1 12) (end 1 17) (annotation error) (line-text "import json.Json"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 13 19) (end 13 22))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "req")
			(reflow " is defined here and then never used."))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_req")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "can_import_unresolved_qualified.md") (start 13 19) (end 13 22) (annotation error) (line-text "processRequest = |req| Http.Server.defaultResponse"))))
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
		(title "Does Not Exist")
		(region (start 13 24) (end 13 51))
		(headline
			(annotated symbol-unqualified "Http.Server.defaultResponse")
			(reflow " does not exist."))
		(document
			(source-region (file "can_import_unresolved_qualified.md") (start 13 24) (end 13 51) (annotation error) (line-text "processRequest = |req| Http.Server.defaultResponse"))))
	(report
		(severity runtime_error)
		(title "Does Not Exist")
		(region (start 22 10) (end 22 28))
		(headline
			(annotated symbol-unqualified "Http.invalidMethod")
			(reflow " does not exist."))
		(document
			(source-region (file "can_import_unresolved_qualified.md") (start 22 10) (end 22 28) (annotation error) (line-text "client = Http.invalidMethod")))))
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
		(e-runtime-error (tag "builtin_import_conflict")))
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
		(e-runtime-error (tag "qualified_ident_does_not_exist")))
	(d-let
		(p-assign (ident "parser"))
		(e-runtime-error (tag "builtin_import_conflict")))
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

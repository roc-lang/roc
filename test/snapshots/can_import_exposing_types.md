# META
~~~ini
description=Import types using exposing syntax
type=snippet
~~~
# SOURCE
~~~roc
import json.Json exposing [Value, Error, Config]
import http.Client as Http exposing [Request, Response, Status]
import utils.Try exposing [Try]

# Test using exposed types directly in annotations
parseJson : Str -> Try(Value, Error)
parseJson = |input| Json.parse(input)

# Test mixing exposed types with qualified access
handleRequest : Request -> Response
handleRequest = |req| {
    result = Json.decode(req.body)
    match result {
        Ok(value) => Http.ok(value)
        Err(error) => Http.badRequest(error)
    }
}

# Test using exposed types in complex signatures
processData : Config, List(Value) -> Try(List(Value), Error)
processData = |config, values|
    List.mapTry(
        values,
        |v| Json.validateWith(config, v),
    )

# Test exposed types in record fields
ServerConfig : {
    jsonConfig : Config,
    httpStatus : Status,
    defaultResponse : Response,
}

# Test exposed types with mod-qualified usage
createClient : Config -> Http.Client
createClient = |config| Http.clientWith(config)

# Test nested type usage
handleResponse : Response -> Str
handleResponse = |response|
    match response.status {
        Ok(status) => Http.statusToString(status)
        Err(error) => Error.toString(error)
    }

# Test mixing exposed and qualified in same expression
combineTrys : Try(Value, Error), Status -> Try(Response, Error)
combineTrys = |jsonTry, httpStatus|
    match jsonTry {
        Ok(value) => Ok({ body: Json.to_str(value), status: httpStatus })
        Err(error) => Err(error)
    }
~~~
# EXPECTED
DUPLICATE DEFINITION - can_import_exposing_types.md:1:1:1:49
UNDECLARED TYPE - can_import_exposing_types.md:6:24:6:29
UNDECLARED TYPE - can_import_exposing_types.md:6:31:6:36
NAME NOT IN SCOPE - can_import_exposing_types.md:7:21:7:31
UNDECLARED TYPE - can_import_exposing_types.md:10:17:10:24
UNDECLARED TYPE - can_import_exposing_types.md:10:28:10:36
NAME NOT IN SCOPE - can_import_exposing_types.md:12:14:12:25
NAME NOT IN SCOPE - can_import_exposing_types.md:14:22:14:29
NAME NOT IN SCOPE - can_import_exposing_types.md:15:23:15:38
UNDECLARED TYPE - can_import_exposing_types.md:20:15:20:21
UNDECLARED TYPE - can_import_exposing_types.md:20:28:20:33
UNDECLARED TYPE - can_import_exposing_types.md:20:47:20:52
UNDECLARED TYPE - can_import_exposing_types.md:20:55:20:60
DOES NOT EXIST - can_import_exposing_types.md:22:5:22:16
NAME NOT IN SCOPE - can_import_exposing_types.md:24:13:24:30
UNDECLARED TYPE - can_import_exposing_types.md:29:18:29:24
UNDECLARED TYPE - can_import_exposing_types.md:30:18:30:24
UNDECLARED TYPE - can_import_exposing_types.md:31:23:31:31
UNDECLARED TYPE - can_import_exposing_types.md:35:16:35:22
MOD NOT FOUND - can_import_exposing_types.md:35:30:35:37
NAME NOT IN SCOPE - can_import_exposing_types.md:36:25:36:40
UNDECLARED TYPE - can_import_exposing_types.md:39:18:39:26
NAME NOT IN SCOPE - can_import_exposing_types.md:42:23:42:42
DOES NOT EXIST - can_import_exposing_types.md:43:23:43:37
UNDECLARED TYPE - can_import_exposing_types.md:47:19:47:24
UNDECLARED TYPE - can_import_exposing_types.md:47:26:47:31
UNDECLARED TYPE - can_import_exposing_types.md:47:34:47:40
UNDECLARED TYPE - can_import_exposing_types.md:47:48:47:56
UNDECLARED TYPE - can_import_exposing_types.md:47:58:47:63
NAME NOT IN SCOPE - can_import_exposing_types.md:50:33:50:44
TYPE MISMATCH - can_import_exposing_types.md:50:22:50:74
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Duplicate Definition")
		(region (start 1 1) (end 1 49))
		(headline
			(reflow "The name ")
			(annotated symbol-unqualified "Json")
			(reflow " is being redeclared here:"))
		(document
			(source-region (file "can_import_exposing_types.md") (start 1 1) (end 1 49) (annotation error) (line-text "import json.Json exposing [Value, Error, Config]"))
			(line-break)
			(reflow "In this scope, ")
			(annotated symbol-unqualified "Json")
			(reflow " was already defined in ")
			(source-location
				(file "can_import_exposing_types.md")
				(line 1)
				(column 1))
			(reflow ":")
			(line-break)
			(source-region (file "can_import_exposing_types.md") (start 1 1) (end 1 1) (annotation dim) (line-text "import json.Json exposing [Value, Error, Config]"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 6 24) (end 6 29))
		(headline
			(reflow "The type ")
			(annotated code "Value")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "can_import_exposing_types.md") (start 6 24) (end 6 29) (annotation error) (line-text "parseJson : Str -> Try(Value, Error)"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 6 31) (end 6 36))
		(headline
			(reflow "The type ")
			(annotated code "Error")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "can_import_exposing_types.md") (start 6 31) (end 6 36) (annotation error) (line-text "parseJson : Str -> Try(Value, Error)"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 7 21) (end 7 31))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "parse")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "can_import_exposing_types.md") (start 7 21) (end 7 31) (annotation error) (line-text "parseJson = |input| Json.parse(input)"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 10 17) (end 10 24))
		(headline
			(reflow "The type ")
			(annotated code "Request")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "can_import_exposing_types.md") (start 10 17) (end 10 24) (annotation error) (line-text "handleRequest : Request -> Response"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 10 28) (end 10 36))
		(headline
			(reflow "The type ")
			(annotated code "Response")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "can_import_exposing_types.md") (start 10 28) (end 10 36) (annotation error) (line-text "handleRequest : Request -> Response"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 12 14) (end 12 25))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "decode")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "can_import_exposing_types.md") (start 12 14) (end 12 25) (annotation error) (line-text "    result = Json.decode(req.body)"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 14 22) (end 14 29))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "ok")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "can_import_exposing_types.md") (start 14 22) (end 14 29) (annotation error) (line-text "        Ok(value) => Http.ok(value)"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 15 23) (end 15 38))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "badRequest")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "can_import_exposing_types.md") (start 15 23) (end 15 38) (annotation error) (line-text "        Err(error) => Http.badRequest(error)"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 20 15) (end 20 21))
		(headline
			(reflow "The type ")
			(annotated code "Config")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "can_import_exposing_types.md") (start 20 15) (end 20 21) (annotation error) (line-text "processData : Config, List(Value) -> Try(List(Value), Error)"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 20 28) (end 20 33))
		(headline
			(reflow "The type ")
			(annotated code "Value")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "can_import_exposing_types.md") (start 20 28) (end 20 33) (annotation error) (line-text "processData : Config, List(Value) -> Try(List(Value), Error)"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 20 47) (end 20 52))
		(headline
			(reflow "The type ")
			(annotated code "Value")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "can_import_exposing_types.md") (start 20 47) (end 20 52) (annotation error) (line-text "processData : Config, List(Value) -> Try(List(Value), Error)"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 20 55) (end 20 60))
		(headline
			(reflow "The type ")
			(annotated code "Error")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "can_import_exposing_types.md") (start 20 55) (end 20 60) (annotation error) (line-text "processData : Config, List(Value) -> Try(List(Value), Error)"))))
	(report
		(severity runtime_error)
		(title "Does Not Exist")
		(region (start 22 5) (end 22 16))
		(headline
			(annotated code "List.mapTry")
			(reflow " does not exist."))
		(document
			(annotated code "List")
			(reflow " is in scope, but it has no associated ")
			(annotated code "mapTry")
			(reflow ".")
			(line-break)
			(line-break)
			(source-region (file "can_import_exposing_types.md") (start 22 5) (end 22 16) (annotation error) (line-text "    List.mapTry("))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 24 13) (end 24 30))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "validateWith")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "can_import_exposing_types.md") (start 24 13) (end 24 30) (annotation error) (line-text "        |v| Json.validateWith(config, v),"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 29 18) (end 29 24))
		(headline
			(reflow "The type ")
			(annotated code "Config")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "can_import_exposing_types.md") (start 29 18) (end 29 24) (annotation error) (line-text "    jsonConfig : Config,"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 30 18) (end 30 24))
		(headline
			(reflow "The type ")
			(annotated code "Status")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "can_import_exposing_types.md") (start 30 18) (end 30 24) (annotation error) (line-text "    httpStatus : Status,"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 31 23) (end 31 31))
		(headline
			(reflow "The type ")
			(annotated code "Response")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "can_import_exposing_types.md") (start 31 23) (end 31 31) (annotation error) (line-text "    defaultResponse : Response,"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 35 16) (end 35 22))
		(headline
			(reflow "The type ")
			(annotated code "Config")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "can_import_exposing_types.md") (start 35 16) (end 35 22) (annotation error) (line-text "createClient : Config -> Http.Client"))))
	(report
		(severity runtime_error)
		(title "Mod Not Found")
		(region (start 35 30) (end 35 37))
		(headline
			(text "This ")
			(annotated code "Client")
			(reflow " type is declared to be in ")
			(annotated code "http.Client")
			(reflow ", which does not exist."))
		(document
			(source-region (file "can_import_exposing_types.md") (start 35 30) (end 35 37) (annotation error) (line-text "createClient : Config -> Http.Client"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 36 25) (end 36 40))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "clientWith")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "can_import_exposing_types.md") (start 36 25) (end 36 40) (annotation error) (line-text "createClient = |config| Http.clientWith(config)"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 39 18) (end 39 26))
		(headline
			(reflow "The type ")
			(annotated code "Response")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "can_import_exposing_types.md") (start 39 18) (end 39 26) (annotation error) (line-text "handleResponse : Response -> Str"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 42 23) (end 42 42))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "statusToString")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "can_import_exposing_types.md") (start 42 23) (end 42 42) (annotation error) (line-text "        Ok(status) => Http.statusToString(status)"))))
	(report
		(severity runtime_error)
		(title "Does Not Exist")
		(region (start 43 23) (end 43 37))
		(headline
			(annotated symbol-unqualified "Error.toString")
			(reflow " does not exist."))
		(document
			(source-region (file "can_import_exposing_types.md") (start 43 23) (end 43 37) (annotation error) (line-text "        Err(error) => Error.toString(error)"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 47 19) (end 47 24))
		(headline
			(reflow "The type ")
			(annotated code "Value")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "can_import_exposing_types.md") (start 47 19) (end 47 24) (annotation error) (line-text "combineTrys : Try(Value, Error), Status -> Try(Response, Error)"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 47 26) (end 47 31))
		(headline
			(reflow "The type ")
			(annotated code "Error")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "can_import_exposing_types.md") (start 47 26) (end 47 31) (annotation error) (line-text "combineTrys : Try(Value, Error), Status -> Try(Response, Error)"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 47 34) (end 47 40))
		(headline
			(reflow "The type ")
			(annotated code "Status")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "can_import_exposing_types.md") (start 47 34) (end 47 40) (annotation error) (line-text "combineTrys : Try(Value, Error), Status -> Try(Response, Error)"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 47 48) (end 47 56))
		(headline
			(reflow "The type ")
			(annotated code "Response")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "can_import_exposing_types.md") (start 47 48) (end 47 56) (annotation error) (line-text "combineTrys : Try(Value, Error), Status -> Try(Response, Error)"))))
	(report
		(severity runtime_error)
		(title "Undeclared Type")
		(region (start 47 58) (end 47 63))
		(headline
			(reflow "The type ")
			(annotated code "Error")
			(reflow " is not declared in this scope."))
		(document
			(source-region (file "can_import_exposing_types.md") (start 47 58) (end 47 63) (annotation error) (line-text "combineTrys : Try(Value, Error), Status -> Try(Response, Error)"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 50 33) (end 50 44))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "to_str")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "can_import_exposing_types.md") (start 50 33) (end 50 44) (annotation error) (line-text "        Ok(value) => Ok({ body: Json.to_str(value), status: httpStatus })"))))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 50 22) (end 50 74))
		(headline
			(reflow "The")
			(reflow " ")
			(reflow "first")
			(reflow " ")
			(reflow "branch of this")
			(reflow " ")
			(annotated code "match")
			(reflow " ")
			(reflow "does not match the previous")
			(reflow " ")
			(reflow "branch")
			(reflow " ")
			(reflow "."))
		(document
			(source-region (file "can_import_exposing_types.md") (start 50 22) (end 50 74) (annotation error) (line-text "        Ok(value) => Ok({ body: Json.to_str(value), status: httpStatus })"))
			(line-break)
			(reflow "The")
			(reflow " ")
			(reflow "first")
			(reflow " ")
			(reflow "branch is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[Ok({ body: Error, status: Error }), ..]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But the previous")
			(reflow " ")
			(reflow "branch results")
			(reflow " ")
			(reflow "in:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Try(Error, Error)")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "All branches in a")
			(reflow " ")
			(annotated code "match")
			(reflow " ")
			(reflow "must have compatible types.")
			(line-break)
			(annotated underline "Note:")
			(reflow " ")
			(reflow "You can wrap branches values in a tag to make them compatible.")
			(line-break)
			(reflow "To learn about tags, see")
			(reflow " ")
			(link "https://www.roc-lang.org/tutorial#tags"))))
~~~
# TOKENS
~~~zig
KwImport,LowerIdent,NoSpaceDotUpperIdent,KwExposing,OpenSquare,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,CloseSquare,
KwImport,LowerIdent,NoSpaceDotUpperIdent,KwAs,UpperIdent,KwExposing,OpenSquare,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,CloseSquare,
KwImport,LowerIdent,NoSpaceDotUpperIdent,KwExposing,OpenSquare,UpperIdent,CloseSquare,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,UpperIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,NoSpaceDotLowerIdent,CloseRound,
KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseCurly,
CloseCurly,
LowerIdent,OpColon,UpperIdent,Comma,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,Comma,UpperIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,
UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,
LowerIdent,Comma,
OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,Comma,
CloseRound,
UpperIdent,OpColon,OpenCurly,
LowerIdent,OpColon,UpperIdent,Comma,
LowerIdent,OpColon,UpperIdent,Comma,
LowerIdent,OpColon,UpperIdent,Comma,
CloseCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,
KwMatch,LowerIdent,NoSpaceDotLowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,UpperIdent,CloseRound,Comma,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,UpperIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,
KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,UpperIdent,NoSpaceOpenRound,OpenCurly,LowerIdent,OpColon,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,LowerIdent,OpColon,LowerIdent,CloseCurly,CloseRound,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpFatArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
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
				(exposed-upper-ident (text "Value"))
				(exposed-upper-ident (text "Error"))
				(exposed-upper-ident (text "Config"))))
		(s-import (raw "http.Client") (alias "Http")
			(exposing
				(exposed-upper-ident (text "Request"))
				(exposed-upper-ident (text "Response"))
				(exposed-upper-ident (text "Status"))))
		(s-import (raw "utils.Try")
			(exposing
				(exposed-upper-ident (text "Try"))))
		(s-type-anno (name "parseJson")
			(ty-fn
				(ty (name "Str"))
				(ty-apply
					(ty (name "Try"))
					(ty (name "Value"))
					(ty (name "Error")))))
		(s-decl
			(p-ident (raw "parseJson"))
			(e-lambda
				(args
					(p-ident (raw "input")))
				(e-apply
					(e-ident (raw "Json.parse"))
					(e-ident (raw "input")))))
		(s-type-anno (name "handleRequest")
			(ty-fn
				(ty (name "Request"))
				(ty (name "Response"))))
		(s-decl
			(p-ident (raw "handleRequest"))
			(e-lambda
				(args
					(p-ident (raw "req")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "result"))
							(e-apply
								(e-ident (raw "Json.decode"))
								(e-field-access
									(receiver
										(e-ident (raw "req")))
									(segment (mode "required") (field "body")))))
						(e-match
							(e-ident (raw "result"))
							(branches
								(branch
									(p-tag (raw "Ok")
										(p-ident (raw "value")))
									(e-apply
										(e-ident (raw "Http.ok"))
										(e-ident (raw "value"))))
								(branch
									(p-tag (raw "Err")
										(p-ident (raw "error")))
									(e-apply
										(e-ident (raw "Http.badRequest"))
										(e-ident (raw "error"))))))))))
		(s-type-anno (name "processData")
			(ty-fn
				(ty (name "Config"))
				(ty-apply
					(ty (name "List"))
					(ty (name "Value")))
				(ty-apply
					(ty (name "Try"))
					(ty-apply
						(ty (name "List"))
						(ty (name "Value")))
					(ty (name "Error")))))
		(s-decl
			(p-ident (raw "processData"))
			(e-lambda
				(args
					(p-ident (raw "config"))
					(p-ident (raw "values")))
				(e-apply
					(e-ident (raw "List.mapTry"))
					(e-ident (raw "values"))
					(e-lambda
						(args
							(p-ident (raw "v")))
						(e-apply
							(e-ident (raw "Json.validateWith"))
							(e-ident (raw "config"))
							(e-ident (raw "v")))))))
		(s-type-decl
			(header (name "ServerConfig")
				(args))
			(ty-record
				(anno-record-field (name "jsonConfig")
					(ty (name "Config")))
				(anno-record-field (name "httpStatus")
					(ty (name "Status")))
				(anno-record-field (name "defaultResponse")
					(ty (name "Response")))))
		(s-type-anno (name "createClient")
			(ty-fn
				(ty (name "Config"))
				(ty (name "Http.Client"))))
		(s-decl
			(p-ident (raw "createClient"))
			(e-lambda
				(args
					(p-ident (raw "config")))
				(e-apply
					(e-ident (raw "Http.clientWith"))
					(e-ident (raw "config")))))
		(s-type-anno (name "handleResponse")
			(ty-fn
				(ty (name "Response"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "handleResponse"))
			(e-lambda
				(args
					(p-ident (raw "response")))
				(e-match
					(e-field-access
						(receiver
							(e-ident (raw "response")))
						(segment (mode "required") (field "status")))
					(branches
						(branch
							(p-tag (raw "Ok")
								(p-ident (raw "status")))
							(e-apply
								(e-ident (raw "Http.statusToString"))
								(e-ident (raw "status"))))
						(branch
							(p-tag (raw "Err")
								(p-ident (raw "error")))
							(e-apply
								(e-ident (raw "Error.toString"))
								(e-ident (raw "error"))))))))
		(s-type-anno (name "combineTrys")
			(ty-fn
				(ty-apply
					(ty (name "Try"))
					(ty (name "Value"))
					(ty (name "Error")))
				(ty (name "Status"))
				(ty-apply
					(ty (name "Try"))
					(ty (name "Response"))
					(ty (name "Error")))))
		(s-decl
			(p-ident (raw "combineTrys"))
			(e-lambda
				(args
					(p-ident (raw "jsonTry"))
					(p-ident (raw "httpStatus")))
				(e-match
					(e-ident (raw "jsonTry"))
					(branches
						(branch
							(p-tag (raw "Ok")
								(p-ident (raw "value")))
							(e-apply
								(e-tag (raw "Ok"))
								(e-record
									(field (field "body")
										(e-apply
											(e-ident (raw "Json.to_str"))
											(e-ident (raw "value"))))
									(field (field "status")
										(e-ident (raw "httpStatus"))))))
						(branch
							(p-tag (raw "Err")
								(p-ident (raw "error")))
							(e-apply
								(e-tag (raw "Err"))
								(e-ident (raw "error"))))))))))
~~~
# FORMATTED
~~~roc
import json.Json exposing [Value, Error, Config]
import http.Client as Http exposing [Request, Response, Status]
import utils.Try exposing [Try]

# Test using exposed types directly in annotations
parseJson : Str -> Try(Value, Error)
parseJson = |input| Json.parse(input)

# Test mixing exposed types with qualified access
handleRequest : Request -> Response
handleRequest = |req| {
	result = Json.decode(req.body)
	match result {
		Ok(value) => Http.ok(value)
		Err(error) => Http.badRequest(error)
	}
}

# Test using exposed types in complex signatures
processData : Config, List(Value) -> Try(List(Value), Error)
processData = |config, values|
	List.mapTry(
		values,
		|v| Json.validateWith(config, v),
	)

# Test exposed types in record fields
ServerConfig : {
	jsonConfig : Config,
	httpStatus : Status,
	defaultResponse : Response,
}

# Test exposed types with mod-qualified usage
createClient : Config -> Http.Client
createClient = |config| Http.clientWith(config)

# Test nested type usage
handleResponse : Response -> Str
handleResponse = |response|
	match response.status {
		Ok(status) => Http.statusToString(status)
		Err(error) => Error.toString(error)
	}

# Test mixing exposed and qualified in same expression
combineTrys : Try(Value, Error), Status -> Try(Response, Error)
combineTrys = |jsonTry, httpStatus|
	match jsonTry {
		Ok(value) => Ok({ body: Json.to_str(value), status: httpStatus })
		Err(error) => Err(error)
	}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "parseJson"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-apply (name "Try") (builtin)
					(ty-malformed)
					(ty-malformed)))))
	(d-let
		(p-assign (ident "handleRequest"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-malformed)
				(ty-malformed))))
	(d-let
		(p-assign (ident "processData"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-malformed)
				(ty-apply (name "List") (builtin)
					(ty-malformed))
				(ty-apply (name "Try") (builtin)
					(ty-apply (name "List") (builtin)
						(ty-malformed))
					(ty-malformed)))))
	(d-let
		(p-assign (ident "createClient"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-malformed)
				(ty-malformed))))
	(d-let
		(p-assign (ident "handleResponse"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-malformed)
				(ty-lookup (name "Str") (builtin)))))
	(d-let
		(p-assign (ident "combineTrys"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "Try") (builtin)
					(ty-malformed)
					(ty-malformed))
				(ty-malformed)
				(ty-apply (name "Try") (builtin)
					(ty-malformed)
					(ty-malformed)))))
	(s-import (mod "json.Json")
		(exposes
			(exposed (name "Value") (wildcard false))
			(exposed (name "Error") (wildcard false))
			(exposed (name "Config") (wildcard false))))
	(s-import (mod "http.Client")
		(exposes
			(exposed (name "Request") (wildcard false))
			(exposed (name "Response") (wildcard false))
			(exposed (name "Status") (wildcard false))))
	(s-import (mod "utils.Try")
		(exposes
			(exposed (name "Try") (wildcard false))))
	(s-alias-decl
		(ty-header (name "ServerConfig"))
		(ty-record
			(field (field "jsonConfig")
				(ty-malformed))
			(field (field "httpStatus")
				(ty-malformed))
			(field (field "defaultResponse")
				(ty-malformed)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str -> Try(Error, Error)"))
		(patt (type "Error -> Error"))
		(patt (type "Error, List(Error) -> Try(List(Error), Error)"))
		(patt (type "Error -> Error"))
		(patt (type "Error -> Str"))
		(patt (type "Try(Error, Error), Error -> Try(Error, Error)")))
	(type_decls
		(alias (type "Error")
			(ty-header (name "ServerConfig"))))
	(expressions
		(expr (type "Str -> Try(Error, Error)"))
		(expr (type "Error -> Error"))
		(expr (type "Error, List(Error) -> Try(List(Error), Error)"))
		(expr (type "Error -> Error"))
		(expr (type "Error -> Str"))
		(expr (type "Try(Error, Error), Error -> Try(Error, Error)"))))
~~~

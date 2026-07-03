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

# Test exposed types with module-qualified usage
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
        Ok(value) => Ok({ body: Json.encode(value), status: httpStatus })
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
MODULE NOT FOUND - can_import_exposing_types.md:35:30:35:37
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
# PROBLEMS

┌──────────────────────┐
│ DUPLICATE DEFINITION ├─ The name `Json` is being redeclared here. ──────────┐
└┬─────────────────────┘                                                      │
 │                                                                            │
 │  import json.Json exposing [Value, Error, Config]                          │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                          │
 └────────────────────────────────────────── can_import_exposing_types.md:1:1 ┘

    In this scope, `Json` was already defined here:
      ┌───────────────────────────────────────────────────────────────────────┐
    1 │  import json.Json exposing [Value, Error, Config]                     │
      │  ‾                                                                    │
      └───────────────────────────────────── can_import_exposing_types.md:1:1 ┘


┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `Value` is not declared in this scope. ─────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  parseJson : Str -> Try(Value, Error)                                      │
 │                         ‾‾‾‾‾                                              │
 └───────────────────────────────────────── can_import_exposing_types.md:6:24 ┘



┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `Error` is not declared in this scope. ─────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  parseJson : Str -> Try(Value, Error)                                      │
 │                                ‾‾‾‾‾                                       │
 └───────────────────────────────────────── can_import_exposing_types.md:6:31 ┘



┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `parse` in this scope. ───────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  parseJson = |input| Json.parse(input)                                     │
 │                      ‾‾‾‾‾‾‾‾‾‾                                            │
 └───────────────────────────────────────── can_import_exposing_types.md:7:21 ┘

    Is it misspelled, or is there an import missing?


┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `Request` is not declared in this scope. ───────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  handleRequest : Request -> Response                                       │
 │                  ‾‾‾‾‾‾‾                                                   │
 └──────────────────────────────────────── can_import_exposing_types.md:10:17 ┘



┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `Response` is not declared in this scope. ──────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  handleRequest : Request -> Response                                       │
 │                             ‾‾‾‾‾‾‾‾                                       │
 └──────────────────────────────────────── can_import_exposing_types.md:10:28 ┘



┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `decode` in this scope. ──────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  result = Json.decode(req.body)                                            │
 │           ‾‾‾‾‾‾‾‾‾‾‾                                                      │
 └──────────────────────────────────────── can_import_exposing_types.md:12:14 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `ok` in this scope. ──────────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  Ok(value) => Http.ok(value)                                               │
 │               ‾‾‾‾‾‾‾                                                      │
 └──────────────────────────────────────── can_import_exposing_types.md:14:22 ┘

    Is it misspelled, or is there an import missing?


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `badRequest` in this scope. ──────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  Err(error) => Http.badRequest(error)                                      │
 │                ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                             │
 └──────────────────────────────────────── can_import_exposing_types.md:15:23 ┘

    Is it misspelled, or is there an import missing?


┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `Config` is not declared in this scope. ────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  processData : Config, List(Value) -> Try(List(Value), Error)              │
 │                ‾‾‾‾‾‾                                                      │
 └──────────────────────────────────────── can_import_exposing_types.md:20:15 ┘



┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `Value` is not declared in this scope. ─────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  processData : Config, List(Value) -> Try(List(Value), Error)              │
 │                             ‾‾‾‾‾                                          │
 └──────────────────────────────────────── can_import_exposing_types.md:20:28 ┘



┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `Value` is not declared in this scope. ─────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  processData : Config, List(Value) -> Try(List(Value), Error)              │
 │                                                ‾‾‾‾‾                       │
 └──────────────────────────────────────── can_import_exposing_types.md:20:47 ┘



┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `Error` is not declared in this scope. ─────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  processData : Config, List(Value) -> Try(List(Value), Error)              │
 │                                                        ‾‾‾‾‾               │
 └──────────────────────────────────────── can_import_exposing_types.md:20:55 ┘



┌────────────────┐
│ DOES NOT EXIST ├─ `List.mapTry` does not exist. ────────────────────────────┐
└┬───────────────┘                                                            │
 │                                                                            │
 │  List.mapTry(                                                              │
 │  ‾‾‾‾‾‾‾‾‾‾‾                                                               │
 └───────────────────────────────────────── can_import_exposing_types.md:22:5 ┘

    `List` is in scope, but it has no associated `mapTry`.


┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `validateWith` in this scope. ────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  |v| Json.validateWith(config, v),                                         │
 │      ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                     │
 └──────────────────────────────────────── can_import_exposing_types.md:24:13 ┘

    Is it misspelled, or is there an import missing?


┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `Config` is not declared in this scope. ────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  jsonConfig : Config,                                                      │
 │               ‾‾‾‾‾‾                                                       │
 └──────────────────────────────────────── can_import_exposing_types.md:29:18 ┘



┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `Status` is not declared in this scope. ────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  httpStatus : Status,                                                      │
 │               ‾‾‾‾‾‾                                                       │
 └──────────────────────────────────────── can_import_exposing_types.md:30:18 ┘



┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `Response` is not declared in this scope. ──────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  defaultResponse : Response,                                               │
 │                    ‾‾‾‾‾‾‾‾                                                │
 └──────────────────────────────────────── can_import_exposing_types.md:31:23 ┘



┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `Config` is not declared in this scope. ────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  createClient : Config -> Http.Client                                      │
 │                 ‾‾‾‾‾‾                                                     │
 └──────────────────────────────────────── can_import_exposing_types.md:35:16 ┘



┌──────────────────┐
│ MODULE NOT FOUND ├─ This `Client` type is declared to be in ────────────────┐
└┬─────────────────┘  `http.Client`, which does not exist.                    │
 │                                                                            │
 │  createClient : Config -> Http.Client                                      │
 │                               ‾‾‾‾‾‾‾                                      │
 └──────────────────────────────────────── can_import_exposing_types.md:35:30 ┘



┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `clientWith` in this scope. ──────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  createClient = |config| Http.clientWith(config)                           │
 │                          ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                   │
 └──────────────────────────────────────── can_import_exposing_types.md:36:25 ┘

    Is it misspelled, or is there an import missing?


┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `Response` is not declared in this scope. ──────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  handleResponse : Response -> Str                                          │
 │                   ‾‾‾‾‾‾‾‾                                                 │
 └──────────────────────────────────────── can_import_exposing_types.md:39:18 ┘



┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `statusToString` in this scope. ──────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  Ok(status) => Http.statusToString(status)                                 │
 │                ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                         │
 └──────────────────────────────────────── can_import_exposing_types.md:42:23 ┘

    Is it misspelled, or is there an import missing?


┌────────────────┐
│ DOES NOT EXIST ├─ `Error.toString` does not exist. ─────────────────────────┐
└┬───────────────┘                                                            │
 │                                                                            │
 │  Err(error) => Error.toString(error)                                       │
 │                ‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                              │
 └──────────────────────────────────────── can_import_exposing_types.md:43:23 ┘



┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `Value` is not declared in this scope. ─────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  combineTrys : Try(Value, Error), Status -> Try(Response, Error)           │
 │                    ‾‾‾‾‾                                                   │
 └──────────────────────────────────────── can_import_exposing_types.md:47:19 ┘



┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `Error` is not declared in this scope. ─────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  combineTrys : Try(Value, Error), Status -> Try(Response, Error)           │
 │                           ‾‾‾‾‾                                            │
 └──────────────────────────────────────── can_import_exposing_types.md:47:26 ┘



┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `Status` is not declared in this scope. ────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  combineTrys : Try(Value, Error), Status -> Try(Response, Error)           │
 │                                   ‾‾‾‾‾‾                                   │
 └──────────────────────────────────────── can_import_exposing_types.md:47:34 ┘



┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `Response` is not declared in this scope. ──────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  combineTrys : Try(Value, Error), Status -> Try(Response, Error)           │
 │                                                 ‾‾‾‾‾‾‾‾                   │
 └──────────────────────────────────────── can_import_exposing_types.md:47:48 ┘



┌─────────────────┐
│ UNDECLARED TYPE ├─ The type `Error` is not declared in this scope. ─────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  combineTrys : Try(Value, Error), Status -> Try(Response, Error)           │
 │                                                           ‾‾‾‾‾            │
 └──────────────────────────────────────── can_import_exposing_types.md:47:58 ┘



┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `encode` in this scope. ──────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  Ok(value) => Ok({ body: Json.encode(value), status: httpStatus })         │
 │                          ‾‾‾‾‾‾‾‾‾‾‾                                       │
 └──────────────────────────────────────── can_import_exposing_types.md:50:33 ┘

    Is it misspelled, or is there an import missing?

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
	(type-module)
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
									(e-ident (raw "req"))
									(e-ident (raw "body")))))
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
						(e-ident (raw "response"))
						(e-ident (raw "status")))
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
											(e-ident (raw "Json.encode"))
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

# Test exposed types with module-qualified usage
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
		Ok(value) => Ok({ body: Json.encode(value), status: httpStatus })
		Err(error) => Err(error)
	}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "parseJson"))
		(e-lambda
			(args
				(p-assign (ident "input")))
			(e-call
				(e-runtime-error (tag "ident_not_in_scope"))
				(e-lookup-local
					(p-assign (ident "input")))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-apply (name "Try") (builtin)
					(ty-malformed)
					(ty-malformed)))))
	(d-let
		(p-assign (ident "handleRequest"))
		(e-lambda
			(args
				(p-assign (ident "req")))
			(e-block
				(s-let
					(p-assign (ident "result"))
					(e-call
						(e-runtime-error (tag "ident_not_in_scope"))
						(e-field-access (field "body")
							(receiver
								(e-lookup-local
									(p-assign (ident "req")))))))
				(e-match
					(match
						(cond
							(e-lookup-local
								(p-assign (ident "result"))))
						(branches
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag)))
								(value
									(e-call
										(e-runtime-error (tag "ident_not_in_scope"))
										(e-lookup-local
											(p-assign (ident "value"))))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag)))
								(value
									(e-call
										(e-runtime-error (tag "ident_not_in_scope"))
										(e-lookup-local
											(p-assign (ident "error")))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-malformed)
				(ty-malformed))))
	(d-let
		(p-assign (ident "processData"))
		(e-lambda
			(args
				(p-assign (ident "config"))
				(p-assign (ident "values")))
			(e-call
				(e-runtime-error (tag "nested_value_not_found"))
				(e-lookup-local
					(p-assign (ident "values")))
				(e-closure
					(captures
						(capture (ident "config")))
					(e-lambda
						(args
							(p-assign (ident "v")))
						(e-call
							(e-runtime-error (tag "ident_not_in_scope"))
							(e-lookup-local
								(p-assign (ident "config")))
							(e-lookup-local
								(p-assign (ident "v"))))))))
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
		(e-lambda
			(args
				(p-assign (ident "config")))
			(e-call
				(e-runtime-error (tag "ident_not_in_scope"))
				(e-lookup-local
					(p-assign (ident "config")))))
		(annotation
			(ty-fn (effectful false)
				(ty-malformed)
				(ty-malformed))))
	(d-let
		(p-assign (ident "handleResponse"))
		(e-lambda
			(args
				(p-assign (ident "response")))
			(e-match
				(match
					(cond
						(e-field-access (field "status")
							(receiver
								(e-lookup-local
									(p-assign (ident "response"))))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-call
									(e-runtime-error (tag "ident_not_in_scope"))
									(e-lookup-local
										(p-assign (ident "status"))))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-call
									(e-runtime-error (tag "qualified_ident_does_not_exist"))
									(e-lookup-local
										(p-assign (ident "error"))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-malformed)
				(ty-lookup (name "Str") (builtin)))))
	(d-let
		(p-assign (ident "combineTrys"))
		(e-lambda
			(args
				(p-assign (ident "jsonTry"))
				(p-assign (ident "httpStatus")))
			(e-match
				(match
					(cond
						(e-lookup-local
							(p-assign (ident "jsonTry"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-tag (name "Ok")
									(args
										(e-record
											(fields
												(field (name "body")
													(e-call
														(e-runtime-error (tag "ident_not_in_scope"))
														(e-lookup-local
															(p-assign (ident "value")))))
												(field (name "status")
													(e-lookup-local
														(p-assign (ident "httpStatus"))))))))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-tag (name "Err")
									(args
										(e-lookup-local
											(p-assign (ident "error")))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "Try") (builtin)
					(ty-malformed)
					(ty-malformed))
				(ty-malformed)
				(ty-apply (name "Try") (builtin)
					(ty-malformed)
					(ty-malformed)))))
	(s-import (module "json.Json")
		(exposes
			(exposed (name "Value") (wildcard false))
			(exposed (name "Error") (wildcard false))
			(exposed (name "Config") (wildcard false))))
	(s-import (module "http.Client")
		(exposes
			(exposed (name "Request") (wildcard false))
			(exposed (name "Response") (wildcard false))
			(exposed (name "Status") (wildcard false))))
	(s-import (module "utils.Try")
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
		(alias (type "ServerConfig")
			(ty-header (name "ServerConfig"))))
	(expressions
		(expr (type "Str -> Try(Error, Error)"))
		(expr (type "Error -> Error"))
		(expr (type "Error, List(Error) -> Try(List(Error), Error)"))
		(expr (type "Error -> Error"))
		(expr (type "Error -> Str"))
		(expr (type "Try(Error, Error), Error -> Try(Error, Error)"))))
~~~

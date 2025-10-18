# META
~~~ini
description=Import types using exposing syntax
type=snippet
~~~
# SOURCE
~~~roc
import json.Json exposing [Value, Error, Config]
import http.Client as Http exposing [Request, Response, Status]
import utils.Result exposing [Result]

# Test using exposed types directly in annotations
parseJson : Str -> Result(Value, Error)
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
processData : Config, List(Value) -> Result(List(Value), Error)
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
combineResults : Result(Value, Error), Status -> Result(Response, Error)
combineResults = |jsonResult, httpStatus|
    match jsonResult {
        Ok(value) => Ok({ body: Json.encode(value), status: httpStatus })
        Err(error) => Err(error)
    }
~~~
# EXPECTED
UNDECLARED TYPE - can_import_exposing_types.md:29:18:29:24
UNDECLARED TYPE - can_import_exposing_types.md:30:18:30:24
UNDECLARED TYPE - can_import_exposing_types.md:31:23:31:31
MODULE NOT FOUND - can_import_exposing_types.md:1:1:1:49
MODULE NOT FOUND - can_import_exposing_types.md:2:1:2:64
DUPLICATE DEFINITION - can_import_exposing_types.md:1:1:1:1
MODULE NOT FOUND - can_import_exposing_types.md:3:1:3:38
UNDECLARED TYPE - can_import_exposing_types.md:6:27:6:32
UNDECLARED TYPE - can_import_exposing_types.md:6:34:6:39
UNDEFINED VARIABLE - can_import_exposing_types.md:7:21:7:31
UNDECLARED TYPE - can_import_exposing_types.md:10:17:10:24
UNDECLARED TYPE - can_import_exposing_types.md:10:28:10:36
UNDEFINED VARIABLE - can_import_exposing_types.md:12:14:12:25
UNDEFINED VARIABLE - can_import_exposing_types.md:14:22:14:29
UNDEFINED VARIABLE - can_import_exposing_types.md:15:23:15:38
UNDECLARED TYPE - can_import_exposing_types.md:20:15:20:21
UNDECLARED TYPE - can_import_exposing_types.md:20:28:20:33
UNDECLARED TYPE - can_import_exposing_types.md:20:50:20:55
UNDECLARED TYPE - can_import_exposing_types.md:20:58:20:63
UNDEFINED VARIABLE - can_import_exposing_types.md:22:5:22:16
UNDEFINED VARIABLE - can_import_exposing_types.md:24:13:24:30
UNDECLARED TYPE - can_import_exposing_types.md:35:16:35:22
UNDEFINED VARIABLE - can_import_exposing_types.md:36:25:36:40
UNDECLARED TYPE - can_import_exposing_types.md:39:18:39:26
UNDEFINED VARIABLE - can_import_exposing_types.md:42:23:42:42
UNDEFINED VARIABLE - can_import_exposing_types.md:43:23:43:37
UNDECLARED TYPE - can_import_exposing_types.md:47:25:47:30
UNDECLARED TYPE - can_import_exposing_types.md:47:32:47:37
UNDECLARED TYPE - can_import_exposing_types.md:47:40:47:46
UNDECLARED TYPE - can_import_exposing_types.md:47:57:47:65
UNDECLARED TYPE - can_import_exposing_types.md:47:67:47:72
UNDEFINED VARIABLE - can_import_exposing_types.md:50:33:50:44
# PROBLEMS
**UNDECLARED TYPE**
The type _Config_ is not declared in this scope.

This type is referenced here:
**can_import_exposing_types.md:29:18:29:24:**
```roc
    jsonConfig : Config,
```
                 ^^^^^^


**UNDECLARED TYPE**
The type _Status_ is not declared in this scope.

This type is referenced here:
**can_import_exposing_types.md:30:18:30:24:**
```roc
    httpStatus : Status,
```
                 ^^^^^^


**UNDECLARED TYPE**
The type _Response_ is not declared in this scope.

This type is referenced here:
**can_import_exposing_types.md:31:23:31:31:**
```roc
    defaultResponse : Response,
```
                      ^^^^^^^^


**MODULE NOT FOUND**
The module `json.Json` was not found in this Roc project.

You're attempting to use this module here:
**can_import_exposing_types.md:1:1:1:49:**
```roc
import json.Json exposing [Value, Error, Config]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**MODULE NOT FOUND**
The module `http.Client` was not found in this Roc project.

You're attempting to use this module here:
**can_import_exposing_types.md:2:1:2:64:**
```roc
import http.Client as Http exposing [Request, Response, Status]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**DUPLICATE DEFINITION**
The name `Result` is being redeclared in this scope.

The redeclaration is here:
**can_import_exposing_types.md:1:1:1:1:**
```roc
import json.Json exposing [Value, Error, Config]
```
^

But `Result` was already defined here:
**can_import_exposing_types.md:1:1:1:1:**
```roc
import json.Json exposing [Value, Error, Config]
```
^


**MODULE NOT FOUND**
The module `utils.Result` was not found in this Roc project.

You're attempting to use this module here:
**can_import_exposing_types.md:3:1:3:38:**
```roc
import utils.Result exposing [Result]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNDECLARED TYPE**
The type _Value_ is not declared in this scope.

This type is referenced here:
**can_import_exposing_types.md:6:27:6:32:**
```roc
parseJson : Str -> Result(Value, Error)
```
                          ^^^^^


**UNDECLARED TYPE**
The type _Error_ is not declared in this scope.

This type is referenced here:
**can_import_exposing_types.md:6:34:6:39:**
```roc
parseJson : Str -> Result(Value, Error)
```
                                 ^^^^^


**UNDEFINED VARIABLE**
Nothing is named `Json.parse` in this scope.
Is there an `import` or `exposing` missing up-top?

**can_import_exposing_types.md:7:21:7:31:**
```roc
parseJson = |input| Json.parse(input)
```
                    ^^^^^^^^^^


**UNDECLARED TYPE**
The type _Request_ is not declared in this scope.

This type is referenced here:
**can_import_exposing_types.md:10:17:10:24:**
```roc
handleRequest : Request -> Response
```
                ^^^^^^^


**UNDECLARED TYPE**
The type _Response_ is not declared in this scope.

This type is referenced here:
**can_import_exposing_types.md:10:28:10:36:**
```roc
handleRequest : Request -> Response
```
                           ^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `Json.decode` in this scope.
Is there an `import` or `exposing` missing up-top?

**can_import_exposing_types.md:12:14:12:25:**
```roc
    result = Json.decode(req.body)
```
             ^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `Http.ok` in this scope.
Is there an `import` or `exposing` missing up-top?

**can_import_exposing_types.md:14:22:14:29:**
```roc
        Ok(value) => Http.ok(value)
```
                     ^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `Http.badRequest` in this scope.
Is there an `import` or `exposing` missing up-top?

**can_import_exposing_types.md:15:23:15:38:**
```roc
        Err(error) => Http.badRequest(error)
```
                      ^^^^^^^^^^^^^^^


**UNDECLARED TYPE**
The type _Config_ is not declared in this scope.

This type is referenced here:
**can_import_exposing_types.md:20:15:20:21:**
```roc
processData : Config, List(Value) -> Result(List(Value), Error)
```
              ^^^^^^


**UNDECLARED TYPE**
The type _Value_ is not declared in this scope.

This type is referenced here:
**can_import_exposing_types.md:20:28:20:33:**
```roc
processData : Config, List(Value) -> Result(List(Value), Error)
```
                           ^^^^^


**UNDECLARED TYPE**
The type _Value_ is not declared in this scope.

This type is referenced here:
**can_import_exposing_types.md:20:50:20:55:**
```roc
processData : Config, List(Value) -> Result(List(Value), Error)
```
                                                 ^^^^^


**UNDECLARED TYPE**
The type _Error_ is not declared in this scope.

This type is referenced here:
**can_import_exposing_types.md:20:58:20:63:**
```roc
processData : Config, List(Value) -> Result(List(Value), Error)
```
                                                         ^^^^^


**UNDEFINED VARIABLE**
Nothing is named `mapTry` in this scope.
Is there an `import` or `exposing` missing up-top?

**can_import_exposing_types.md:22:5:22:16:**
```roc
    List.mapTry(
```
    ^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `Json.validateWith` in this scope.
Is there an `import` or `exposing` missing up-top?

**can_import_exposing_types.md:24:13:24:30:**
```roc
        |v| Json.validateWith(config, v),
```
            ^^^^^^^^^^^^^^^^^


**UNDECLARED TYPE**
The type _Config_ is not declared in this scope.

This type is referenced here:
**can_import_exposing_types.md:35:16:35:22:**
```roc
createClient : Config -> Http.Client
```
               ^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `Http.clientWith` in this scope.
Is there an `import` or `exposing` missing up-top?

**can_import_exposing_types.md:36:25:36:40:**
```roc
createClient = |config| Http.clientWith(config)
```
                        ^^^^^^^^^^^^^^^


**UNDECLARED TYPE**
The type _Response_ is not declared in this scope.

This type is referenced here:
**can_import_exposing_types.md:39:18:39:26:**
```roc
handleResponse : Response -> Str
```
                 ^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `Http.statusToString` in this scope.
Is there an `import` or `exposing` missing up-top?

**can_import_exposing_types.md:42:23:42:42:**
```roc
        Ok(status) => Http.statusToString(status)
```
                      ^^^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `toString` in this scope.
Is there an `import` or `exposing` missing up-top?

**can_import_exposing_types.md:43:23:43:37:**
```roc
        Err(error) => Error.toString(error)
```
                      ^^^^^^^^^^^^^^


**UNDECLARED TYPE**
The type _Value_ is not declared in this scope.

This type is referenced here:
**can_import_exposing_types.md:47:25:47:30:**
```roc
combineResults : Result(Value, Error), Status -> Result(Response, Error)
```
                        ^^^^^


**UNDECLARED TYPE**
The type _Error_ is not declared in this scope.

This type is referenced here:
**can_import_exposing_types.md:47:32:47:37:**
```roc
combineResults : Result(Value, Error), Status -> Result(Response, Error)
```
                               ^^^^^


**UNDECLARED TYPE**
The type _Status_ is not declared in this scope.

This type is referenced here:
**can_import_exposing_types.md:47:40:47:46:**
```roc
combineResults : Result(Value, Error), Status -> Result(Response, Error)
```
                                       ^^^^^^


**UNDECLARED TYPE**
The type _Response_ is not declared in this scope.

This type is referenced here:
**can_import_exposing_types.md:47:57:47:65:**
```roc
combineResults : Result(Value, Error), Status -> Result(Response, Error)
```
                                                        ^^^^^^^^


**UNDECLARED TYPE**
The type _Error_ is not declared in this scope.

This type is referenced here:
**can_import_exposing_types.md:47:67:47:72:**
```roc
combineResults : Result(Value, Error), Status -> Result(Response, Error)
```
                                                                  ^^^^^


**UNDEFINED VARIABLE**
Nothing is named `Json.encode` in this scope.
Is there an `import` or `exposing` missing up-top?

**can_import_exposing_types.md:50:33:50:44:**
```roc
        Ok(value) => Ok({ body: Json.encode(value), status: httpStatus })
```
                                ^^^^^^^^^^^


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
		(s-import (raw "utils.Result")
			(exposing
				(exposed-upper-ident (text "Result"))))
		(s-type-anno (name "parseJson")
			(ty-fn
				(ty (name "Str"))
				(ty-apply
					(ty (name "Result"))
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
					(ty (name "Result"))
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
		(s-type-anno (name "combineResults")
			(ty-fn
				(ty-apply
					(ty (name "Result"))
					(ty (name "Value"))
					(ty (name "Error")))
				(ty (name "Status"))
				(ty-apply
					(ty (name "Result"))
					(ty (name "Response"))
					(ty (name "Error")))))
		(s-decl
			(p-ident (raw "combineResults"))
			(e-lambda
				(args
					(p-ident (raw "jsonResult"))
					(p-ident (raw "httpStatus")))
				(e-match
					(e-ident (raw "jsonResult"))
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
import utils.Result exposing [Result]

# Test using exposed types directly in annotations
parseJson : Str -> Result(Value, Error)
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
processData : Config, List(Value) -> Result(List(Value), Error)
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
combineResults : Result(Value, Error), Status -> Result(Response, Error)
combineResults = |jsonResult, httpStatus|
	match jsonResult {
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
			(declared-type
				(ty-fn (effectful false)
					(ty-lookup (name "Str") (builtin))
					(ty-apply (name "Result") (external (module-idx "3") (target-node-idx "3"))
						(ty-malformed)
						(ty-malformed))))))
	(d-let
		(p-assign (ident "handleRequest"))
		(e-closure
			(captures
				(capture (ident "error"))
				(capture (ident "value")))
			(e-lambda
				(args
					(p-assign (ident "req")))
				(e-block
					(s-let
						(p-assign (ident "result"))
						(e-call
							(e-runtime-error (tag "ident_not_in_scope"))
							(e-dot-access (field "body")
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
												(p-assign (ident "error"))))))))))))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-malformed)
					(ty-malformed)))))
	(d-let
		(p-assign (ident "processData"))
		(e-lambda
			(args
				(p-assign (ident "config"))
				(p-assign (ident "values")))
			(e-call
				(e-runtime-error (tag "ident_not_in_scope"))
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
			(declared-type
				(ty-fn (effectful false)
					(ty-malformed)
					(ty-apply (name "List") (builtin)
						(ty-malformed))
					(ty-apply (name "Result") (external (module-idx "3") (target-node-idx "3"))
						(ty-apply (name "List") (builtin)
							(ty-malformed))
						(ty-malformed))))))
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
			(declared-type
				(ty-fn (effectful false)
					(ty-malformed)
					(ty-lookup (name "Client") (external (module-idx "5") (target-node-idx "0")))))))
	(d-let
		(p-assign (ident "handleResponse"))
		(e-closure
			(captures
				(capture (ident "status"))
				(capture (ident "error")))
			(e-lambda
				(args
					(p-assign (ident "response")))
				(e-match
					(match
						(cond
							(e-dot-access (field "status")
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
										(e-runtime-error (tag "ident_not_in_scope"))
										(e-lookup-local
											(p-assign (ident "error")))))))))))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-malformed)
					(ty-lookup (name "Str") (builtin))))))
	(d-let
		(p-assign (ident "combineResults"))
		(e-closure
			(captures
				(capture (ident "value"))
				(capture (ident "error")))
			(e-lambda
				(args
					(p-assign (ident "jsonResult"))
					(p-assign (ident "httpStatus")))
				(e-match
					(match
						(cond
							(e-lookup-local
								(p-assign (ident "jsonResult"))))
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
												(p-assign (ident "error"))))))))))))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-apply (name "Result") (external (module-idx "3") (target-node-idx "3"))
						(ty-malformed)
						(ty-malformed))
					(ty-malformed)
					(ty-apply (name "Result") (external (module-idx "3") (target-node-idx "3"))
						(ty-malformed)
						(ty-malformed))))))
	(s-alias-decl
		(ty-header (name "ServerConfig"))
		(ty-record
			(field (field "jsonConfig")
				(ty-malformed))
			(field (field "httpStatus")
				(ty-malformed))
			(field (field "defaultResponse")
				(ty-malformed))))
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
	(s-import (module "utils.Result")
		(exposes
			(exposed (name "Result") (wildcard false)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str -> Result(Error, Error)"))
		(patt (type "Error -> Error"))
		(patt (type "Error, List(Error) -> Result(List(Error), Error)"))
		(patt (type "Error -> Error"))
		(patt (type "Error -> Str"))
		(patt (type "Result(Error, Error), Error -> Result(Error, Error)")))
	(type_decls
		(alias (type "ServerConfig")
			(ty-header (name "ServerConfig"))))
	(expressions
		(expr (type "Str -> Result(Error, Error)"))
		(expr (type "Error -> Error"))
		(expr (type "Error, List(Error) -> Result(List(Error), Error)"))
		(expr (type "Error -> Error"))
		(expr (type "Error -> Str"))
		(expr (type "Result(Error, Error), Error -> Result(Error, Error)"))))
~~~

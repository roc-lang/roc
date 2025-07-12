# META
~~~ini
description=Import types using exposing syntax
type=file
~~~
# SOURCE
~~~roc
module []

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
UNDECLARED TYPE - can_import_exposing_types.md:31:18:31:24
UNDECLARED TYPE - can_import_exposing_types.md:32:18:32:24
UNDECLARED TYPE - can_import_exposing_types.md:33:23:33:31
UNDECLARED TYPE - can_import_exposing_types.md:8:27:8:32
UNDECLARED TYPE - can_import_exposing_types.md:8:34:8:39
UNDECLARED TYPE - can_import_exposing_types.md:12:17:12:24
UNDECLARED TYPE - can_import_exposing_types.md:12:28:12:36
UNDECLARED TYPE - can_import_exposing_types.md:22:15:22:21
UNDECLARED TYPE - can_import_exposing_types.md:22:28:22:33
UNDECLARED TYPE - can_import_exposing_types.md:22:50:22:55
UNDECLARED TYPE - can_import_exposing_types.md:22:58:22:63
UNDEFINED VARIABLE - can_import_exposing_types.md:24:5:24:16
UNDECLARED TYPE - can_import_exposing_types.md:37:16:37:22
UNDECLARED TYPE - can_import_exposing_types.md:41:18:41:26
UNDEFINED VARIABLE - can_import_exposing_types.md:45:23:45:37
UNDECLARED TYPE - can_import_exposing_types.md:49:25:49:30
UNDECLARED TYPE - can_import_exposing_types.md:49:32:49:37
UNDECLARED TYPE - can_import_exposing_types.md:49:40:49:46
UNDECLARED TYPE - can_import_exposing_types.md:49:57:49:65
UNDECLARED TYPE - can_import_exposing_types.md:49:67:49:72
# PROBLEMS
**UNDECLARED TYPE**
The type ``Config`` is not declared in this scope.

This type is referenced here:
**can_import_exposing_types.md:31:18:31:24:**
```roc
    jsonConfig : Config,
```
                 ^^^^^^


**UNDECLARED TYPE**
The type ``Status`` is not declared in this scope.

This type is referenced here:
**can_import_exposing_types.md:32:18:32:24:**
```roc
    httpStatus : Status,
```
                 ^^^^^^


**UNDECLARED TYPE**
The type ``Response`` is not declared in this scope.

This type is referenced here:
**can_import_exposing_types.md:33:23:33:31:**
```roc
    defaultResponse : Response,
```
                      ^^^^^^^^


**UNDECLARED TYPE**
The type ``Value`` is not declared in this scope.

This type is referenced here:
**can_import_exposing_types.md:8:27:8:32:**
```roc
parseJson : Str -> Result(Value, Error)
```
                          ^^^^^


**UNDECLARED TYPE**
The type ``Error`` is not declared in this scope.

This type is referenced here:
**can_import_exposing_types.md:8:34:8:39:**
```roc
parseJson : Str -> Result(Value, Error)
```
                                 ^^^^^


**UNDECLARED TYPE**
The type ``Request`` is not declared in this scope.

This type is referenced here:
**can_import_exposing_types.md:12:17:12:24:**
```roc
handleRequest : Request -> Response
```
                ^^^^^^^


**UNDECLARED TYPE**
The type ``Response`` is not declared in this scope.

This type is referenced here:
**can_import_exposing_types.md:12:28:12:36:**
```roc
handleRequest : Request -> Response
```
                           ^^^^^^^^


**UNDECLARED TYPE**
The type ``Config`` is not declared in this scope.

This type is referenced here:
**can_import_exposing_types.md:22:15:22:21:**
```roc
processData : Config, List(Value) -> Result(List(Value), Error)
```
              ^^^^^^


**UNDECLARED TYPE**
The type ``Value`` is not declared in this scope.

This type is referenced here:
**can_import_exposing_types.md:22:28:22:33:**
```roc
processData : Config, List(Value) -> Result(List(Value), Error)
```
                           ^^^^^


**UNDECLARED TYPE**
The type ``Value`` is not declared in this scope.

This type is referenced here:
**can_import_exposing_types.md:22:50:22:55:**
```roc
processData : Config, List(Value) -> Result(List(Value), Error)
```
                                                 ^^^^^


**UNDECLARED TYPE**
The type ``Error`` is not declared in this scope.

This type is referenced here:
**can_import_exposing_types.md:22:58:22:63:**
```roc
processData : Config, List(Value) -> Result(List(Value), Error)
```
                                                         ^^^^^


**UNDEFINED VARIABLE**
Nothing is named `mapTry` in this scope.
Is there an `import` or `exposing` missing up-top?

**can_import_exposing_types.md:24:5:24:16:**
```roc
    List.mapTry(
```
    ^^^^^^^^^^^


**UNDECLARED TYPE**
The type ``Config`` is not declared in this scope.

This type is referenced here:
**can_import_exposing_types.md:37:16:37:22:**
```roc
createClient : Config -> Http.Client
```
               ^^^^^^


**UNDECLARED TYPE**
The type ``Response`` is not declared in this scope.

This type is referenced here:
**can_import_exposing_types.md:41:18:41:26:**
```roc
handleResponse : Response -> Str
```
                 ^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named `toString` in this scope.
Is there an `import` or `exposing` missing up-top?

**can_import_exposing_types.md:45:23:45:37:**
```roc
        Err(error) => Error.toString(error)
```
                      ^^^^^^^^^^^^^^


**UNDECLARED TYPE**
The type ``Value`` is not declared in this scope.

This type is referenced here:
**can_import_exposing_types.md:49:25:49:30:**
```roc
combineResults : Result(Value, Error), Status -> Result(Response, Error)
```
                        ^^^^^


**UNDECLARED TYPE**
The type ``Error`` is not declared in this scope.

This type is referenced here:
**can_import_exposing_types.md:49:32:49:37:**
```roc
combineResults : Result(Value, Error), Status -> Result(Response, Error)
```
                               ^^^^^


**UNDECLARED TYPE**
The type ``Status`` is not declared in this scope.

This type is referenced here:
**can_import_exposing_types.md:49:40:49:46:**
```roc
combineResults : Result(Value, Error), Status -> Result(Response, Error)
```
                                       ^^^^^^


**UNDECLARED TYPE**
The type ``Response`` is not declared in this scope.

This type is referenced here:
**can_import_exposing_types.md:49:57:49:65:**
```roc
combineResults : Result(Value, Error), Status -> Result(Response, Error)
```
                                                        ^^^^^^^^


**UNDECLARED TYPE**
The type ``Error`` is not declared in this scope.

This type is referenced here:
**can_import_exposing_types.md:49:67:49:72:**
```roc
combineResults : Result(Value, Error), Status -> Result(Response, Error)
```
                                                                  ^^^^^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
KwImport(3:1-3:7),LowerIdent(3:8-3:12),NoSpaceDotUpperIdent(3:12-3:17),KwExposing(3:18-3:26),OpenSquare(3:27-3:28),UpperIdent(3:28-3:33),Comma(3:33-3:34),UpperIdent(3:35-3:40),Comma(3:40-3:41),UpperIdent(3:42-3:48),CloseSquare(3:48-3:49),
KwImport(4:1-4:7),LowerIdent(4:8-4:12),NoSpaceDotUpperIdent(4:12-4:19),KwAs(4:20-4:22),UpperIdent(4:23-4:27),KwExposing(4:28-4:36),OpenSquare(4:37-4:38),UpperIdent(4:38-4:45),Comma(4:45-4:46),UpperIdent(4:47-4:55),Comma(4:55-4:56),UpperIdent(4:57-4:63),CloseSquare(4:63-4:64),
KwImport(5:1-5:7),LowerIdent(5:8-5:13),NoSpaceDotUpperIdent(5:13-5:20),KwExposing(5:21-5:29),OpenSquare(5:30-5:31),UpperIdent(5:31-5:37),CloseSquare(5:37-5:38),
LowerIdent(8:1-8:10),OpColon(8:11-8:12),UpperIdent(8:13-8:16),OpArrow(8:17-8:19),UpperIdent(8:20-8:26),NoSpaceOpenRound(8:26-8:27),UpperIdent(8:27-8:32),Comma(8:32-8:33),UpperIdent(8:34-8:39),CloseRound(8:39-8:40),
LowerIdent(9:1-9:10),OpAssign(9:11-9:12),OpBar(9:13-9:14),LowerIdent(9:14-9:19),OpBar(9:19-9:20),UpperIdent(9:21-9:25),NoSpaceDotLowerIdent(9:25-9:31),NoSpaceOpenRound(9:31-9:32),LowerIdent(9:32-9:37),CloseRound(9:37-9:38),
LowerIdent(12:1-12:14),OpColon(12:15-12:16),UpperIdent(12:17-12:24),OpArrow(12:25-12:27),UpperIdent(12:28-12:36),
LowerIdent(13:1-13:14),OpAssign(13:15-13:16),OpBar(13:17-13:18),LowerIdent(13:18-13:21),OpBar(13:21-13:22),OpenCurly(13:23-13:24),
LowerIdent(14:5-14:11),OpAssign(14:12-14:13),UpperIdent(14:14-14:18),NoSpaceDotLowerIdent(14:18-14:25),NoSpaceOpenRound(14:25-14:26),LowerIdent(14:26-14:29),NoSpaceDotLowerIdent(14:29-14:34),CloseRound(14:34-14:35),
KwMatch(15:5-15:10),LowerIdent(15:11-15:17),OpenCurly(15:18-15:19),
UpperIdent(16:9-16:11),NoSpaceOpenRound(16:11-16:12),LowerIdent(16:12-16:17),CloseRound(16:17-16:18),OpFatArrow(16:19-16:21),UpperIdent(16:22-16:26),NoSpaceDotLowerIdent(16:26-16:29),NoSpaceOpenRound(16:29-16:30),LowerIdent(16:30-16:35),CloseRound(16:35-16:36),
UpperIdent(17:9-17:12),NoSpaceOpenRound(17:12-17:13),LowerIdent(17:13-17:18),CloseRound(17:18-17:19),OpFatArrow(17:20-17:22),UpperIdent(17:23-17:27),NoSpaceDotLowerIdent(17:27-17:38),NoSpaceOpenRound(17:38-17:39),LowerIdent(17:39-17:44),CloseRound(17:44-17:45),
CloseCurly(18:5-18:6),
CloseCurly(19:1-19:2),
LowerIdent(22:1-22:12),OpColon(22:13-22:14),UpperIdent(22:15-22:21),Comma(22:21-22:22),UpperIdent(22:23-22:27),NoSpaceOpenRound(22:27-22:28),UpperIdent(22:28-22:33),CloseRound(22:33-22:34),OpArrow(22:35-22:37),UpperIdent(22:38-22:44),NoSpaceOpenRound(22:44-22:45),UpperIdent(22:45-22:49),NoSpaceOpenRound(22:49-22:50),UpperIdent(22:50-22:55),CloseRound(22:55-22:56),Comma(22:56-22:57),UpperIdent(22:58-22:63),CloseRound(22:63-22:64),
LowerIdent(23:1-23:12),OpAssign(23:13-23:14),OpBar(23:15-23:16),LowerIdent(23:16-23:22),Comma(23:22-23:23),LowerIdent(23:24-23:30),OpBar(23:30-23:31),
UpperIdent(24:5-24:9),NoSpaceDotLowerIdent(24:9-24:16),NoSpaceOpenRound(24:16-24:17),
LowerIdent(25:9-25:15),Comma(25:15-25:16),
OpBar(26:9-26:10),LowerIdent(26:10-26:11),OpBar(26:11-26:12),UpperIdent(26:13-26:17),NoSpaceDotLowerIdent(26:17-26:30),NoSpaceOpenRound(26:30-26:31),LowerIdent(26:31-26:37),Comma(26:37-26:38),LowerIdent(26:39-26:40),CloseRound(26:40-26:41),Comma(26:41-26:42),
CloseRound(27:5-27:6),
UpperIdent(30:1-30:13),OpColon(30:14-30:15),OpenCurly(30:16-30:17),
LowerIdent(31:5-31:15),OpColon(31:16-31:17),UpperIdent(31:18-31:24),Comma(31:24-31:25),
LowerIdent(32:5-32:15),OpColon(32:16-32:17),UpperIdent(32:18-32:24),Comma(32:24-32:25),
LowerIdent(33:5-33:20),OpColon(33:21-33:22),UpperIdent(33:23-33:31),Comma(33:31-33:32),
CloseCurly(34:1-34:2),
LowerIdent(37:1-37:13),OpColon(37:14-37:15),UpperIdent(37:16-37:22),OpArrow(37:23-37:25),UpperIdent(37:26-37:30),NoSpaceDotUpperIdent(37:30-37:37),
LowerIdent(38:1-38:13),OpAssign(38:14-38:15),OpBar(38:16-38:17),LowerIdent(38:17-38:23),OpBar(38:23-38:24),UpperIdent(38:25-38:29),NoSpaceDotLowerIdent(38:29-38:40),NoSpaceOpenRound(38:40-38:41),LowerIdent(38:41-38:47),CloseRound(38:47-38:48),
LowerIdent(41:1-41:15),OpColon(41:16-41:17),UpperIdent(41:18-41:26),OpArrow(41:27-41:29),UpperIdent(41:30-41:33),
LowerIdent(42:1-42:15),OpAssign(42:16-42:17),OpBar(42:18-42:19),LowerIdent(42:19-42:27),OpBar(42:27-42:28),
KwMatch(43:5-43:10),LowerIdent(43:11-43:19),NoSpaceDotLowerIdent(43:19-43:26),OpenCurly(43:27-43:28),
UpperIdent(44:9-44:11),NoSpaceOpenRound(44:11-44:12),LowerIdent(44:12-44:18),CloseRound(44:18-44:19),OpFatArrow(44:20-44:22),UpperIdent(44:23-44:27),NoSpaceDotLowerIdent(44:27-44:42),NoSpaceOpenRound(44:42-44:43),LowerIdent(44:43-44:49),CloseRound(44:49-44:50),
UpperIdent(45:9-45:12),NoSpaceOpenRound(45:12-45:13),LowerIdent(45:13-45:18),CloseRound(45:18-45:19),OpFatArrow(45:20-45:22),UpperIdent(45:23-45:28),NoSpaceDotLowerIdent(45:28-45:37),NoSpaceOpenRound(45:37-45:38),LowerIdent(45:38-45:43),CloseRound(45:43-45:44),
CloseCurly(46:5-46:6),
LowerIdent(49:1-49:15),OpColon(49:16-49:17),UpperIdent(49:18-49:24),NoSpaceOpenRound(49:24-49:25),UpperIdent(49:25-49:30),Comma(49:30-49:31),UpperIdent(49:32-49:37),CloseRound(49:37-49:38),Comma(49:38-49:39),UpperIdent(49:40-49:46),OpArrow(49:47-49:49),UpperIdent(49:50-49:56),NoSpaceOpenRound(49:56-49:57),UpperIdent(49:57-49:65),Comma(49:65-49:66),UpperIdent(49:67-49:72),CloseRound(49:72-49:73),
LowerIdent(50:1-50:15),OpAssign(50:16-50:17),OpBar(50:18-50:19),LowerIdent(50:19-50:29),Comma(50:29-50:30),LowerIdent(50:31-50:41),OpBar(50:41-50:42),
KwMatch(51:5-51:10),LowerIdent(51:11-51:21),OpenCurly(51:22-51:23),
UpperIdent(52:9-52:11),NoSpaceOpenRound(52:11-52:12),LowerIdent(52:12-52:17),CloseRound(52:17-52:18),OpFatArrow(52:19-52:21),UpperIdent(52:22-52:24),NoSpaceOpenRound(52:24-52:25),OpenCurly(52:25-52:26),LowerIdent(52:27-52:31),OpColon(52:31-52:32),UpperIdent(52:33-52:37),NoSpaceDotLowerIdent(52:37-52:44),NoSpaceOpenRound(52:44-52:45),LowerIdent(52:45-52:50),CloseRound(52:50-52:51),Comma(52:51-52:52),LowerIdent(52:53-52:59),OpColon(52:59-52:60),LowerIdent(52:61-52:71),CloseCurly(52:72-52:73),CloseRound(52:73-52:74),
UpperIdent(53:9-53:12),NoSpaceOpenRound(53:12-53:13),LowerIdent(53:13-53:18),CloseRound(53:18-53:19),OpFatArrow(53:20-53:22),UpperIdent(53:23-53:26),NoSpaceOpenRound(53:26-53:27),LowerIdent(53:27-53:32),CloseRound(53:32-53:33),
CloseCurly(54:5-54:6),EndOfFile(54:6-54:6),
~~~
# PARSE
~~~clojure
(file @1.1-54.6
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-import @3.1-3.49 (raw "json.Json")
			(exposing
				(exposed-upper-ident @3.28-3.33 (text "Value"))
				(exposed-upper-ident @3.35-3.40 (text "Error"))
				(exposed-upper-ident @3.42-3.48 (text "Config"))))
		(s-import @4.1-4.64 (raw "http.Client") (alias "Http")
			(exposing
				(exposed-upper-ident @4.38-4.45 (text "Request"))
				(exposed-upper-ident @4.47-4.55 (text "Response"))
				(exposed-upper-ident @4.57-4.63 (text "Status"))))
		(s-import @5.1-5.38 (raw "utils.Result")
			(exposing
				(exposed-upper-ident @5.31-5.37 (text "Result"))))
		(s-type-anno @8.1-8.40 (name "parseJson")
			(ty-fn @8.13-8.40
				(ty @8.13-8.16 (name "Str"))
				(ty-apply @8.20-8.40
					(ty @8.20-8.26 (name "Result"))
					(ty @8.27-8.32 (name "Value"))
					(ty @8.34-8.39 (name "Error")))))
		(s-decl @9.1-9.38
			(p-ident @9.1-9.10 (raw "parseJson"))
			(e-lambda @9.13-9.38
				(args
					(p-ident @9.14-9.19 (raw "input")))
				(e-apply @9.21-9.38
					(e-ident @9.21-9.31 (raw "Json.parse"))
					(e-ident @9.32-9.37 (raw "input")))))
		(s-type-anno @12.1-12.36 (name "handleRequest")
			(ty-fn @12.17-12.36
				(ty @12.17-12.24 (name "Request"))
				(ty @12.28-12.36 (name "Response"))))
		(s-decl @13.1-19.2
			(p-ident @13.1-13.14 (raw "handleRequest"))
			(e-lambda @13.17-19.2
				(args
					(p-ident @13.18-13.21 (raw "req")))
				(e-block @13.23-19.2
					(statements
						(s-decl @14.5-14.35
							(p-ident @14.5-14.11 (raw "result"))
							(e-apply @14.14-14.35
								(e-ident @14.14-14.25 (raw "Json.decode"))
								(e-field-access @14.26-14.34
									(e-ident @14.26-14.29 (raw "req"))
									(e-ident @14.29-14.34 (raw "body")))))
						(e-match
							(e-ident @15.11-15.17 (raw "result"))
							(branches
								(branch @16.9-16.36
									(p-tag @16.9-16.18 (raw "Ok")
										(p-ident @16.12-16.17 (raw "value")))
									(e-apply @16.22-16.36
										(e-ident @16.22-16.29 (raw "Http.ok"))
										(e-ident @16.30-16.35 (raw "value"))))
								(branch @17.9-17.45
									(p-tag @17.9-17.19 (raw "Err")
										(p-ident @17.13-17.18 (raw "error")))
									(e-apply @17.23-17.45
										(e-ident @17.23-17.38 (raw "Http.badRequest"))
										(e-ident @17.39-17.44 (raw "error"))))))))))
		(s-type-anno @22.1-22.64 (name "processData")
			(ty-fn @22.15-22.64
				(ty @22.15-22.21 (name "Config"))
				(ty-apply @22.23-22.34
					(ty @22.23-22.27 (name "List"))
					(ty @22.28-22.33 (name "Value")))
				(ty-apply @22.38-22.64
					(ty @22.38-22.44 (name "Result"))
					(ty-apply @22.45-22.56
						(ty @22.45-22.49 (name "List"))
						(ty @22.50-22.55 (name "Value")))
					(ty @22.58-22.63 (name "Error")))))
		(s-decl @23.1-27.6
			(p-ident @23.1-23.12 (raw "processData"))
			(e-lambda @23.15-27.6
				(args
					(p-ident @23.16-23.22 (raw "config"))
					(p-ident @23.24-23.30 (raw "values")))
				(e-apply @24.5-27.6
					(e-ident @24.5-24.16 (raw "List.mapTry"))
					(e-ident @25.9-25.15 (raw "values"))
					(e-lambda @26.9-26.41
						(args
							(p-ident @26.10-26.11 (raw "v")))
						(e-apply @26.13-26.41
							(e-ident @26.13-26.30 (raw "Json.validateWith"))
							(e-ident @26.31-26.37 (raw "config"))
							(e-ident @26.39-26.40 (raw "v")))))))
		(s-type-decl @30.1-34.2
			(header @30.1-30.13 (name "ServerConfig")
				(args))
			(ty-record @30.16-34.2
				(anno-record-field @31.5-31.24 (name "jsonConfig")
					(ty @31.18-31.24 (name "Config")))
				(anno-record-field @32.5-32.24 (name "httpStatus")
					(ty @32.18-32.24 (name "Status")))
				(anno-record-field @33.5-33.31 (name "defaultResponse")
					(ty @33.23-33.31 (name "Response")))))
		(s-type-anno @37.1-37.37 (name "createClient")
			(ty-fn @37.16-37.37
				(ty @37.16-37.22 (name "Config"))
				(ty @37.26-37.37 (name "Http.Client"))))
		(s-decl @38.1-38.48
			(p-ident @38.1-38.13 (raw "createClient"))
			(e-lambda @38.16-38.48
				(args
					(p-ident @38.17-38.23 (raw "config")))
				(e-apply @38.25-38.48
					(e-ident @38.25-38.40 (raw "Http.clientWith"))
					(e-ident @38.41-38.47 (raw "config")))))
		(s-type-anno @41.1-41.33 (name "handleResponse")
			(ty-fn @41.18-41.33
				(ty @41.18-41.26 (name "Response"))
				(ty @41.30-41.33 (name "Str"))))
		(s-decl @42.1-46.6
			(p-ident @42.1-42.15 (raw "handleResponse"))
			(e-lambda @42.18-46.6
				(args
					(p-ident @42.19-42.27 (raw "response")))
				(e-match
					(e-field-access @43.11-43.26
						(e-ident @43.11-43.19 (raw "response"))
						(e-ident @43.19-43.26 (raw "status")))
					(branches
						(branch @44.9-44.50
							(p-tag @44.9-44.19 (raw "Ok")
								(p-ident @44.12-44.18 (raw "status")))
							(e-apply @44.23-44.50
								(e-ident @44.23-44.42 (raw "Http.statusToString"))
								(e-ident @44.43-44.49 (raw "status"))))
						(branch @45.9-45.44
							(p-tag @45.9-45.19 (raw "Err")
								(p-ident @45.13-45.18 (raw "error")))
							(e-apply @45.23-45.44
								(e-ident @45.23-45.37 (raw "Error.toString"))
								(e-ident @45.38-45.43 (raw "error"))))))))
		(s-type-anno @49.1-49.73 (name "combineResults")
			(ty-fn @49.18-49.73
				(ty-apply @49.18-49.38
					(ty @49.18-49.24 (name "Result"))
					(ty @49.25-49.30 (name "Value"))
					(ty @49.32-49.37 (name "Error")))
				(ty @49.40-49.46 (name "Status"))
				(ty-apply @49.50-49.73
					(ty @49.50-49.56 (name "Result"))
					(ty @49.57-49.65 (name "Response"))
					(ty @49.67-49.72 (name "Error")))))
		(s-decl @50.1-54.6
			(p-ident @50.1-50.15 (raw "combineResults"))
			(e-lambda @50.18-54.6
				(args
					(p-ident @50.19-50.29 (raw "jsonResult"))
					(p-ident @50.31-50.41 (raw "httpStatus")))
				(e-match
					(e-ident @51.11-51.21 (raw "jsonResult"))
					(branches
						(branch @52.9-52.74
							(p-tag @52.9-52.18 (raw "Ok")
								(p-ident @52.12-52.17 (raw "value")))
							(e-apply @52.22-52.74
								(e-tag @52.22-52.24 (raw "Ok"))
								(e-record @52.25-52.73
									(field (field "body")
										(e-apply @52.33-52.51
											(e-ident @52.33-52.44 (raw "Json.encode"))
											(e-ident @52.45-52.50 (raw "value"))))
									(field (field "status")
										(e-ident @52.61-52.71 (raw "httpStatus"))))))
						(branch @53.9-53.33
							(p-tag @53.9-53.19 (raw "Err")
								(p-ident @53.13-53.18 (raw "error")))
							(e-apply @53.23-53.33
								(e-tag @53.23-53.26 (raw "Err"))
								(e-ident @53.27-53.32 (raw "error"))))))))))
~~~
# FORMATTED
~~~roc
module []

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

	# Test using exposed types in complex signatures
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
		Ok(value) => Ok({body: Json.encode(value), status: httpStatus})
		Err(error) => Err(error)
	}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @9.1-9.10 (ident "parseJson"))
		(e-lambda @9.13-9.38
			(args
				(p-assign @9.14-9.19 (ident "input")))
			(e-call @9.21-9.38
				(e-lookup-external @9.21-9.31
					(module-idx "0")
					(target-node-idx "0"))
				(e-lookup-local @9.32-9.37
					(p-assign @9.14-9.19 (ident "input")))))
		(annotation @9.1-9.10
			(declared-type
				(ty-fn @8.13-8.40 (effectful false)
					(ty @8.13-8.16 (name "Str"))
					(ty-apply @8.20-8.40 (symbol "Result")
						(ty @8.27-8.32 (name "Value"))
						(ty @8.34-8.39 (name "Error")))))))
	(d-let
		(p-assign @13.1-13.14 (ident "handleRequest"))
		(e-lambda @13.17-19.2
			(args
				(p-assign @13.18-13.21 (ident "req")))
			(e-block @13.23-19.2
				(s-let @14.5-14.35
					(p-assign @14.5-14.11 (ident "result"))
					(e-call @14.14-14.35
						(e-lookup-external @14.14-14.25
							(module-idx "0")
							(target-node-idx "0"))
						(e-dot-access @14.26-14.34 (field "body")
							(receiver
								(e-lookup-local @14.26-14.29
									(p-assign @13.18-13.21 (ident "req")))))))
				(e-match @15.5-18.6
					(match @15.5-18.6
						(cond
							(e-lookup-local @15.11-15.17
								(p-assign @14.5-14.11 (ident "result"))))
						(branches
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag @16.9-16.18)))
								(value
									(e-call @16.22-16.36
										(e-lookup-external @16.22-16.29
											(module-idx "1")
											(target-node-idx "0"))
										(e-lookup-local @16.30-16.35
											(p-assign @16.12-16.17 (ident "value"))))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag @17.9-17.19)))
								(value
									(e-call @17.23-17.45
										(e-lookup-external @17.23-17.38
											(module-idx "1")
											(target-node-idx "0"))
										(e-lookup-local @17.39-17.44
											(p-assign @17.13-17.18 (ident "error")))))))))))
		(annotation @13.1-13.14
			(declared-type
				(ty-fn @12.17-12.36 (effectful false)
					(ty @12.17-12.24 (name "Request"))
					(ty @12.28-12.36 (name "Response"))))))
	(d-let
		(p-assign @23.1-23.12 (ident "processData"))
		(e-lambda @23.15-27.6
			(args
				(p-assign @23.16-23.22 (ident "config"))
				(p-assign @23.24-23.30 (ident "values")))
			(e-call @24.5-27.6
				(e-runtime-error (tag "ident_not_in_scope"))
				(e-lookup-local @25.9-25.15
					(p-assign @23.24-23.30 (ident "values")))
				(e-lambda @26.9-26.41
					(args
						(p-assign @26.10-26.11 (ident "v")))
					(e-call @26.13-26.41
						(e-lookup-external @26.13-26.30
							(module-idx "0")
							(target-node-idx "0"))
						(e-lookup-local @26.31-26.37
							(p-assign @23.16-23.22 (ident "config")))
						(e-lookup-local @26.39-26.40
							(p-assign @26.10-26.11 (ident "v")))))))
		(annotation @23.1-23.12
			(declared-type
				(ty-fn @22.15-22.64 (effectful false)
					(ty @22.15-22.21 (name "Config"))
					(ty-apply @22.23-22.34 (symbol "List")
						(ty @22.28-22.33 (name "Value")))
					(ty-apply @22.38-22.64 (symbol "Result")
						(ty-apply @22.45-22.56 (symbol "List")
							(ty @22.50-22.55 (name "Value")))
						(ty @22.58-22.63 (name "Error")))))))
	(d-let
		(p-assign @38.1-38.13 (ident "createClient"))
		(e-lambda @38.16-38.48
			(args
				(p-assign @38.17-38.23 (ident "config")))
			(e-call @38.25-38.48
				(e-lookup-external @38.25-38.40
					(module-idx "1")
					(target-node-idx "0"))
				(e-lookup-local @38.41-38.47
					(p-assign @38.17-38.23 (ident "config")))))
		(annotation @38.1-38.13
			(declared-type
				(ty-fn @37.16-37.37 (effectful false)
					(ty @37.16-37.22 (name "Config"))
					(ty-lookup-external @37.26-37.37
						(ext-decl @37.26-37.37 (ident "Http.Client") (kind "type")))))))
	(d-let
		(p-assign @42.1-42.15 (ident "handleResponse"))
		(e-lambda @42.18-46.6
			(args
				(p-assign @42.19-42.27 (ident "response")))
			(e-match @43.5-46.6
				(match @43.5-46.6
					(cond
						(e-dot-access @43.11-43.26 (field "status")
							(receiver
								(e-lookup-local @43.11-43.19
									(p-assign @42.19-42.27 (ident "response"))))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag @44.9-44.19)))
							(value
								(e-call @44.23-44.50
									(e-lookup-external @44.23-44.42
										(module-idx "1")
										(target-node-idx "0"))
									(e-lookup-local @44.43-44.49
										(p-assign @44.12-44.18 (ident "status"))))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag @45.9-45.19)))
							(value
								(e-call @45.23-45.44
									(e-runtime-error (tag "ident_not_in_scope"))
									(e-lookup-local @45.38-45.43
										(p-assign @45.13-45.18 (ident "error"))))))))))
		(annotation @42.1-42.15
			(declared-type
				(ty-fn @41.18-41.33 (effectful false)
					(ty @41.18-41.26 (name "Response"))
					(ty @41.30-41.33 (name "Str"))))))
	(d-let
		(p-assign @50.1-50.15 (ident "combineResults"))
		(e-lambda @50.18-54.6
			(args
				(p-assign @50.19-50.29 (ident "jsonResult"))
				(p-assign @50.31-50.41 (ident "httpStatus")))
			(e-match @51.5-54.6
				(match @51.5-54.6
					(cond
						(e-lookup-local @51.11-51.21
							(p-assign @50.19-50.29 (ident "jsonResult"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag @52.9-52.18)))
							(value
								(e-tag @52.22-52.24 (name "Ok")
									(args
										(e-record @52.25-52.73
											(fields
												(field (name "body")
													(e-call @52.33-52.51
														(e-lookup-external @52.33-52.44
															(module-idx "0")
															(target-node-idx "0"))
														(e-lookup-local @52.45-52.50
															(p-assign @52.12-52.17 (ident "value")))))
												(field (name "status")
													(e-lookup-local @52.61-52.71
														(p-assign @50.31-50.41 (ident "httpStatus"))))))))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag @53.9-53.19)))
							(value
								(e-tag @53.23-53.26 (name "Err")
									(args
										(e-lookup-local @53.27-53.32
											(p-assign @53.13-53.18 (ident "error")))))))))))
		(annotation @50.1-50.15
			(declared-type
				(ty-fn @49.18-49.73 (effectful false)
					(ty-apply @49.18-49.38 (symbol "Result")
						(ty @49.25-49.30 (name "Value"))
						(ty @49.32-49.37 (name "Error")))
					(ty @49.40-49.46 (name "Status"))
					(ty-apply @49.50-49.73 (symbol "Result")
						(ty @49.57-49.65 (name "Response"))
						(ty @49.67-49.72 (name "Error")))))))
	(s-alias-decl @30.1-34.2
		(ty-header @30.1-30.13 (name "ServerConfig"))
		(ty-record @30.16-34.2
			(field (field "jsonConfig")
				(ty @31.18-31.24 (name "Config")))
			(field (field "httpStatus")
				(ty @32.18-32.24 (name "Status")))
			(field (field "defaultResponse")
				(ty @33.23-33.31 (name "Response")))))
	(s-import @3.1-3.49 (module "json.Json") (qualifier "json")
		(exposes
			(exposed (name "Value") (wildcard false))
			(exposed (name "Error") (wildcard false))
			(exposed (name "Config") (wildcard false))))
	(s-import @4.1-4.64 (module "http.Client") (qualifier "http") (alias "Http")
		(exposes
			(exposed (name "Request") (wildcard false))
			(exposed (name "Response") (wildcard false))
			(exposed (name "Status") (wildcard false))))
	(s-import @5.1-5.38 (module "utils.Result") (qualifier "utils")
		(exposes
			(exposed (name "Result") (wildcard false))))
	(ext-decl @37.26-37.37 (ident "Http.Client") (kind "type")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @9.1-9.10 (type "Str -> Error"))
		(patt @13.1-13.14 (type "Error -> Error"))
		(patt @23.1-23.12 (type "Error, Error -> Error"))
		(patt @38.1-38.13 (type "Error -> Http.Client"))
		(patt @42.1-42.15 (type "Error -> Str"))
		(patt @50.1-50.15 (type "Error, Error -> Error")))
	(type_decls
		(alias @30.1-34.2 (type "ServerConfig")
			(ty-header @30.1-30.13 (name "ServerConfig"))))
	(expressions
		(expr @9.13-9.38 (type "Str -> Error"))
		(expr @13.17-19.2 (type "Error -> Error"))
		(expr @23.15-27.6 (type "Error, Error -> Error"))
		(expr @38.16-38.48 (type "Error -> Http.Client"))
		(expr @42.18-46.6 (type "Error -> Str"))
		(expr @50.18-54.6 (type "Error, Error -> Error"))))
~~~

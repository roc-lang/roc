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
MODULE NOT FOUND - can_import_exposing_types.md:3:1:3:38
UNDECLARED TYPE - can_import_exposing_types.md:6:27:6:32
UNDECLARED TYPE - can_import_exposing_types.md:6:34:6:39
UNDECLARED TYPE - can_import_exposing_types.md:10:17:10:24
UNDECLARED TYPE - can_import_exposing_types.md:10:28:10:36
UNDECLARED TYPE - can_import_exposing_types.md:20:15:20:21
UNDECLARED TYPE - can_import_exposing_types.md:20:28:20:33
UNDECLARED TYPE - can_import_exposing_types.md:20:50:20:55
UNDECLARED TYPE - can_import_exposing_types.md:20:58:20:63
UNDEFINED VARIABLE - can_import_exposing_types.md:22:5:22:16
UNDECLARED TYPE - can_import_exposing_types.md:35:16:35:22
UNDECLARED TYPE - can_import_exposing_types.md:39:18:39:26
UNDEFINED VARIABLE - can_import_exposing_types.md:43:23:43:37
UNDECLARED TYPE - can_import_exposing_types.md:47:25:47:30
UNDECLARED TYPE - can_import_exposing_types.md:47:32:47:37
UNDECLARED TYPE - can_import_exposing_types.md:47:40:47:46
UNDECLARED TYPE - can_import_exposing_types.md:47:57:47:65
UNDECLARED TYPE - can_import_exposing_types.md:47:67:47:72
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


**UNDECLARED TYPE**
The type _Config_ is not declared in this scope.

This type is referenced here:
**can_import_exposing_types.md:35:16:35:22:**
```roc
createClient : Config -> Http.Client
```
               ^^^^^^


**UNDECLARED TYPE**
The type _Response_ is not declared in this scope.

This type is referenced here:
**can_import_exposing_types.md:39:18:39:26:**
```roc
handleResponse : Response -> Str
```
                 ^^^^^^^^


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


# TOKENS
~~~zig
KwImport(1:1-1:7),LowerIdent(1:8-1:12),NoSpaceDotUpperIdent(1:12-1:17),KwExposing(1:18-1:26),OpenSquare(1:27-1:28),UpperIdent(1:28-1:33),Comma(1:33-1:34),UpperIdent(1:35-1:40),Comma(1:40-1:41),UpperIdent(1:42-1:48),CloseSquare(1:48-1:49),
KwImport(2:1-2:7),LowerIdent(2:8-2:12),NoSpaceDotUpperIdent(2:12-2:19),KwAs(2:20-2:22),UpperIdent(2:23-2:27),KwExposing(2:28-2:36),OpenSquare(2:37-2:38),UpperIdent(2:38-2:45),Comma(2:45-2:46),UpperIdent(2:47-2:55),Comma(2:55-2:56),UpperIdent(2:57-2:63),CloseSquare(2:63-2:64),
KwImport(3:1-3:7),LowerIdent(3:8-3:13),NoSpaceDotUpperIdent(3:13-3:20),KwExposing(3:21-3:29),OpenSquare(3:30-3:31),UpperIdent(3:31-3:37),CloseSquare(3:37-3:38),
LowerIdent(6:1-6:10),OpColon(6:11-6:12),UpperIdent(6:13-6:16),OpArrow(6:17-6:19),UpperIdent(6:20-6:26),NoSpaceOpenRound(6:26-6:27),UpperIdent(6:27-6:32),Comma(6:32-6:33),UpperIdent(6:34-6:39),CloseRound(6:39-6:40),
LowerIdent(7:1-7:10),OpAssign(7:11-7:12),OpBar(7:13-7:14),LowerIdent(7:14-7:19),OpBar(7:19-7:20),UpperIdent(7:21-7:25),NoSpaceDotLowerIdent(7:25-7:31),NoSpaceOpenRound(7:31-7:32),LowerIdent(7:32-7:37),CloseRound(7:37-7:38),
LowerIdent(10:1-10:14),OpColon(10:15-10:16),UpperIdent(10:17-10:24),OpArrow(10:25-10:27),UpperIdent(10:28-10:36),
LowerIdent(11:1-11:14),OpAssign(11:15-11:16),OpBar(11:17-11:18),LowerIdent(11:18-11:21),OpBar(11:21-11:22),OpenCurly(11:23-11:24),
LowerIdent(12:5-12:11),OpAssign(12:12-12:13),UpperIdent(12:14-12:18),NoSpaceDotLowerIdent(12:18-12:25),NoSpaceOpenRound(12:25-12:26),LowerIdent(12:26-12:29),NoSpaceDotLowerIdent(12:29-12:34),CloseRound(12:34-12:35),
KwMatch(13:5-13:10),LowerIdent(13:11-13:17),OpenCurly(13:18-13:19),
UpperIdent(14:9-14:11),NoSpaceOpenRound(14:11-14:12),LowerIdent(14:12-14:17),CloseRound(14:17-14:18),OpFatArrow(14:19-14:21),UpperIdent(14:22-14:26),NoSpaceDotLowerIdent(14:26-14:29),NoSpaceOpenRound(14:29-14:30),LowerIdent(14:30-14:35),CloseRound(14:35-14:36),
UpperIdent(15:9-15:12),NoSpaceOpenRound(15:12-15:13),LowerIdent(15:13-15:18),CloseRound(15:18-15:19),OpFatArrow(15:20-15:22),UpperIdent(15:23-15:27),NoSpaceDotLowerIdent(15:27-15:38),NoSpaceOpenRound(15:38-15:39),LowerIdent(15:39-15:44),CloseRound(15:44-15:45),
CloseCurly(16:5-16:6),
CloseCurly(17:1-17:2),
LowerIdent(20:1-20:12),OpColon(20:13-20:14),UpperIdent(20:15-20:21),Comma(20:21-20:22),UpperIdent(20:23-20:27),NoSpaceOpenRound(20:27-20:28),UpperIdent(20:28-20:33),CloseRound(20:33-20:34),OpArrow(20:35-20:37),UpperIdent(20:38-20:44),NoSpaceOpenRound(20:44-20:45),UpperIdent(20:45-20:49),NoSpaceOpenRound(20:49-20:50),UpperIdent(20:50-20:55),CloseRound(20:55-20:56),Comma(20:56-20:57),UpperIdent(20:58-20:63),CloseRound(20:63-20:64),
LowerIdent(21:1-21:12),OpAssign(21:13-21:14),OpBar(21:15-21:16),LowerIdent(21:16-21:22),Comma(21:22-21:23),LowerIdent(21:24-21:30),OpBar(21:30-21:31),
UpperIdent(22:5-22:9),NoSpaceDotLowerIdent(22:9-22:16),NoSpaceOpenRound(22:16-22:17),
LowerIdent(23:9-23:15),Comma(23:15-23:16),
OpBar(24:9-24:10),LowerIdent(24:10-24:11),OpBar(24:11-24:12),UpperIdent(24:13-24:17),NoSpaceDotLowerIdent(24:17-24:30),NoSpaceOpenRound(24:30-24:31),LowerIdent(24:31-24:37),Comma(24:37-24:38),LowerIdent(24:39-24:40),CloseRound(24:40-24:41),Comma(24:41-24:42),
CloseRound(25:5-25:6),
UpperIdent(28:1-28:13),OpColon(28:14-28:15),OpenCurly(28:16-28:17),
LowerIdent(29:5-29:15),OpColon(29:16-29:17),UpperIdent(29:18-29:24),Comma(29:24-29:25),
LowerIdent(30:5-30:15),OpColon(30:16-30:17),UpperIdent(30:18-30:24),Comma(30:24-30:25),
LowerIdent(31:5-31:20),OpColon(31:21-31:22),UpperIdent(31:23-31:31),Comma(31:31-31:32),
CloseCurly(32:1-32:2),
LowerIdent(35:1-35:13),OpColon(35:14-35:15),UpperIdent(35:16-35:22),OpArrow(35:23-35:25),UpperIdent(35:26-35:30),NoSpaceDotUpperIdent(35:30-35:37),
LowerIdent(36:1-36:13),OpAssign(36:14-36:15),OpBar(36:16-36:17),LowerIdent(36:17-36:23),OpBar(36:23-36:24),UpperIdent(36:25-36:29),NoSpaceDotLowerIdent(36:29-36:40),NoSpaceOpenRound(36:40-36:41),LowerIdent(36:41-36:47),CloseRound(36:47-36:48),
LowerIdent(39:1-39:15),OpColon(39:16-39:17),UpperIdent(39:18-39:26),OpArrow(39:27-39:29),UpperIdent(39:30-39:33),
LowerIdent(40:1-40:15),OpAssign(40:16-40:17),OpBar(40:18-40:19),LowerIdent(40:19-40:27),OpBar(40:27-40:28),
KwMatch(41:5-41:10),LowerIdent(41:11-41:19),NoSpaceDotLowerIdent(41:19-41:26),OpenCurly(41:27-41:28),
UpperIdent(42:9-42:11),NoSpaceOpenRound(42:11-42:12),LowerIdent(42:12-42:18),CloseRound(42:18-42:19),OpFatArrow(42:20-42:22),UpperIdent(42:23-42:27),NoSpaceDotLowerIdent(42:27-42:42),NoSpaceOpenRound(42:42-42:43),LowerIdent(42:43-42:49),CloseRound(42:49-42:50),
UpperIdent(43:9-43:12),NoSpaceOpenRound(43:12-43:13),LowerIdent(43:13-43:18),CloseRound(43:18-43:19),OpFatArrow(43:20-43:22),UpperIdent(43:23-43:28),NoSpaceDotLowerIdent(43:28-43:37),NoSpaceOpenRound(43:37-43:38),LowerIdent(43:38-43:43),CloseRound(43:43-43:44),
CloseCurly(44:5-44:6),
LowerIdent(47:1-47:15),OpColon(47:16-47:17),UpperIdent(47:18-47:24),NoSpaceOpenRound(47:24-47:25),UpperIdent(47:25-47:30),Comma(47:30-47:31),UpperIdent(47:32-47:37),CloseRound(47:37-47:38),Comma(47:38-47:39),UpperIdent(47:40-47:46),OpArrow(47:47-47:49),UpperIdent(47:50-47:56),NoSpaceOpenRound(47:56-47:57),UpperIdent(47:57-47:65),Comma(47:65-47:66),UpperIdent(47:67-47:72),CloseRound(47:72-47:73),
LowerIdent(48:1-48:15),OpAssign(48:16-48:17),OpBar(48:18-48:19),LowerIdent(48:19-48:29),Comma(48:29-48:30),LowerIdent(48:31-48:41),OpBar(48:41-48:42),
KwMatch(49:5-49:10),LowerIdent(49:11-49:21),OpenCurly(49:22-49:23),
UpperIdent(50:9-50:11),NoSpaceOpenRound(50:11-50:12),LowerIdent(50:12-50:17),CloseRound(50:17-50:18),OpFatArrow(50:19-50:21),UpperIdent(50:22-50:24),NoSpaceOpenRound(50:24-50:25),OpenCurly(50:25-50:26),LowerIdent(50:27-50:31),OpColon(50:31-50:32),UpperIdent(50:33-50:37),NoSpaceDotLowerIdent(50:37-50:44),NoSpaceOpenRound(50:44-50:45),LowerIdent(50:45-50:50),CloseRound(50:50-50:51),Comma(50:51-50:52),LowerIdent(50:53-50:59),OpColon(50:59-50:60),LowerIdent(50:61-50:71),CloseCurly(50:72-50:73),CloseRound(50:73-50:74),
UpperIdent(51:9-51:12),NoSpaceOpenRound(51:12-51:13),LowerIdent(51:13-51:18),CloseRound(51:18-51:19),OpFatArrow(51:20-51:22),UpperIdent(51:23-51:26),NoSpaceOpenRound(51:26-51:27),LowerIdent(51:27-51:32),CloseRound(51:32-51:33),
CloseCurly(52:5-52:6),
EndOfFile(53:1-53:1),
~~~
# PARSE
~~~clojure
(file @1.1-52.6
	(type-module @1.1-1.7)
	(statements
		(s-import @1.1-1.49 (raw "json.Json")
			(exposing
				(exposed-upper-ident @1.28-1.33 (text "Value"))
				(exposed-upper-ident @1.35-1.40 (text "Error"))
				(exposed-upper-ident @1.42-1.48 (text "Config"))))
		(s-import @2.1-2.64 (raw "http.Client") (alias "Http")
			(exposing
				(exposed-upper-ident @2.38-2.45 (text "Request"))
				(exposed-upper-ident @2.47-2.55 (text "Response"))
				(exposed-upper-ident @2.57-2.63 (text "Status"))))
		(s-import @3.1-3.38 (raw "utils.Result")
			(exposing
				(exposed-upper-ident @3.31-3.37 (text "Result"))))
		(s-type-anno @6.1-6.40 (name "parseJson")
			(ty-fn @6.13-6.40
				(ty @6.13-6.16 (name "Str"))
				(ty-apply @6.20-6.40
					(ty @6.20-6.26 (name "Result"))
					(ty @6.27-6.32 (name "Value"))
					(ty @6.34-6.39 (name "Error")))))
		(s-decl @7.1-7.38
			(p-ident @7.1-7.10 (raw "parseJson"))
			(e-lambda @7.13-7.38
				(args
					(p-ident @7.14-7.19 (raw "input")))
				(e-apply @7.21-7.38
					(e-ident @7.21-7.31 (raw "Json.parse"))
					(e-ident @7.32-7.37 (raw "input")))))
		(s-type-anno @10.1-10.36 (name "handleRequest")
			(ty-fn @10.17-10.36
				(ty @10.17-10.24 (name "Request"))
				(ty @10.28-10.36 (name "Response"))))
		(s-decl @11.1-17.2
			(p-ident @11.1-11.14 (raw "handleRequest"))
			(e-lambda @11.17-17.2
				(args
					(p-ident @11.18-11.21 (raw "req")))
				(e-block @11.23-17.2
					(statements
						(s-decl @12.5-12.35
							(p-ident @12.5-12.11 (raw "result"))
							(e-apply @12.14-12.35
								(e-ident @12.14-12.25 (raw "Json.decode"))
								(e-field-access @12.26-12.34
									(e-ident @12.26-12.29 (raw "req"))
									(e-ident @12.29-12.34 (raw "body")))))
						(e-match
							(e-ident @13.11-13.17 (raw "result"))
							(branches
								(branch @14.9-14.36
									(p-tag @14.9-14.18 (raw "Ok")
										(p-ident @14.12-14.17 (raw "value")))
									(e-apply @14.22-14.36
										(e-ident @14.22-14.29 (raw "Http.ok"))
										(e-ident @14.30-14.35 (raw "value"))))
								(branch @15.9-15.45
									(p-tag @15.9-15.19 (raw "Err")
										(p-ident @15.13-15.18 (raw "error")))
									(e-apply @15.23-15.45
										(e-ident @15.23-15.38 (raw "Http.badRequest"))
										(e-ident @15.39-15.44 (raw "error"))))))))))
		(s-type-anno @20.1-20.64 (name "processData")
			(ty-fn @20.15-20.64
				(ty @20.15-20.21 (name "Config"))
				(ty-apply @20.23-20.34
					(ty @20.23-20.27 (name "List"))
					(ty @20.28-20.33 (name "Value")))
				(ty-apply @20.38-20.64
					(ty @20.38-20.44 (name "Result"))
					(ty-apply @20.45-20.56
						(ty @20.45-20.49 (name "List"))
						(ty @20.50-20.55 (name "Value")))
					(ty @20.58-20.63 (name "Error")))))
		(s-decl @21.1-25.6
			(p-ident @21.1-21.12 (raw "processData"))
			(e-lambda @21.15-25.6
				(args
					(p-ident @21.16-21.22 (raw "config"))
					(p-ident @21.24-21.30 (raw "values")))
				(e-apply @22.5-25.6
					(e-ident @22.5-22.16 (raw "List.mapTry"))
					(e-ident @23.9-23.15 (raw "values"))
					(e-lambda @24.9-24.41
						(args
							(p-ident @24.10-24.11 (raw "v")))
						(e-apply @24.13-24.41
							(e-ident @24.13-24.30 (raw "Json.validateWith"))
							(e-ident @24.31-24.37 (raw "config"))
							(e-ident @24.39-24.40 (raw "v")))))))
		(s-type-decl @28.1-32.2
			(header @28.1-28.13 (name "ServerConfig")
				(args))
			(ty-record @28.16-32.2
				(anno-record-field @29.5-29.24 (name "jsonConfig")
					(ty @29.18-29.24 (name "Config")))
				(anno-record-field @30.5-30.24 (name "httpStatus")
					(ty @30.18-30.24 (name "Status")))
				(anno-record-field @31.5-31.31 (name "defaultResponse")
					(ty @31.23-31.31 (name "Response")))))
		(s-type-anno @35.1-35.37 (name "createClient")
			(ty-fn @35.16-35.37
				(ty @35.16-35.22 (name "Config"))
				(ty @35.26-35.37 (name "Http.Client"))))
		(s-decl @36.1-36.48
			(p-ident @36.1-36.13 (raw "createClient"))
			(e-lambda @36.16-36.48
				(args
					(p-ident @36.17-36.23 (raw "config")))
				(e-apply @36.25-36.48
					(e-ident @36.25-36.40 (raw "Http.clientWith"))
					(e-ident @36.41-36.47 (raw "config")))))
		(s-type-anno @39.1-39.33 (name "handleResponse")
			(ty-fn @39.18-39.33
				(ty @39.18-39.26 (name "Response"))
				(ty @39.30-39.33 (name "Str"))))
		(s-decl @40.1-44.6
			(p-ident @40.1-40.15 (raw "handleResponse"))
			(e-lambda @40.18-44.6
				(args
					(p-ident @40.19-40.27 (raw "response")))
				(e-match
					(e-field-access @41.11-41.26
						(e-ident @41.11-41.19 (raw "response"))
						(e-ident @41.19-41.26 (raw "status")))
					(branches
						(branch @42.9-42.50
							(p-tag @42.9-42.19 (raw "Ok")
								(p-ident @42.12-42.18 (raw "status")))
							(e-apply @42.23-42.50
								(e-ident @42.23-42.42 (raw "Http.statusToString"))
								(e-ident @42.43-42.49 (raw "status"))))
						(branch @43.9-43.44
							(p-tag @43.9-43.19 (raw "Err")
								(p-ident @43.13-43.18 (raw "error")))
							(e-apply @43.23-43.44
								(e-ident @43.23-43.37 (raw "Error.toString"))
								(e-ident @43.38-43.43 (raw "error"))))))))
		(s-type-anno @47.1-47.73 (name "combineResults")
			(ty-fn @47.18-47.73
				(ty-apply @47.18-47.38
					(ty @47.18-47.24 (name "Result"))
					(ty @47.25-47.30 (name "Value"))
					(ty @47.32-47.37 (name "Error")))
				(ty @47.40-47.46 (name "Status"))
				(ty-apply @47.50-47.73
					(ty @47.50-47.56 (name "Result"))
					(ty @47.57-47.65 (name "Response"))
					(ty @47.67-47.72 (name "Error")))))
		(s-decl @48.1-52.6
			(p-ident @48.1-48.15 (raw "combineResults"))
			(e-lambda @48.18-52.6
				(args
					(p-ident @48.19-48.29 (raw "jsonResult"))
					(p-ident @48.31-48.41 (raw "httpStatus")))
				(e-match
					(e-ident @49.11-49.21 (raw "jsonResult"))
					(branches
						(branch @50.9-50.74
							(p-tag @50.9-50.18 (raw "Ok")
								(p-ident @50.12-50.17 (raw "value")))
							(e-apply @50.22-50.74
								(e-tag @50.22-50.24 (raw "Ok"))
								(e-record @50.25-50.73
									(field (field "body")
										(e-apply @50.33-50.51
											(e-ident @50.33-50.44 (raw "Json.encode"))
											(e-ident @50.45-50.50 (raw "value"))))
									(field (field "status")
										(e-ident @50.61-50.71 (raw "httpStatus"))))))
						(branch @51.9-51.33
							(p-tag @51.9-51.19 (raw "Err")
								(p-ident @51.13-51.18 (raw "error")))
							(e-apply @51.23-51.33
								(e-tag @51.23-51.26 (raw "Err"))
								(e-ident @51.27-51.32 (raw "error"))))))))))
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
		(p-assign @7.1-7.10 (ident "parseJson"))
		(e-lambda @7.13-7.38
			(args
				(p-assign @7.14-7.19 (ident "input")))
			(e-call @7.21-7.38
				(e-lookup-external @7.21-7.31
					(module-idx "2")
					(target-node-idx "0"))
				(e-lookup-local @7.32-7.37
					(p-assign @7.14-7.19 (ident "input")))))
		(annotation @7.1-7.10
			(declared-type
				(ty-fn @6.13-6.40 (effectful false)
					(ty-lookup @6.13-6.16 (name "Str") (builtin))
					(ty-apply @6.20-6.40 (name "Result") (local)
						(ty-malformed @6.20-6.40)
						(ty-malformed @6.20-6.40))))))
	(d-let
		(p-assign @11.1-11.14 (ident "handleRequest"))
		(e-closure @11.17-17.2
			(captures
				(capture @14.12-14.17 (ident "value"))
				(capture @15.13-15.18 (ident "error")))
			(e-lambda @11.17-17.2
				(args
					(p-assign @11.18-11.21 (ident "req")))
				(e-block @11.23-17.2
					(s-let @12.5-12.35
						(p-assign @12.5-12.11 (ident "result"))
						(e-call @12.14-12.35
							(e-lookup-external @12.14-12.25
								(module-idx "2")
								(target-node-idx "0"))
							(e-dot-access @12.26-12.34 (field "body")
								(receiver
									(e-lookup-local @12.26-12.29
										(p-assign @11.18-11.21 (ident "req")))))))
					(e-match @13.5-16.6
						(match @13.5-16.6
							(cond
								(e-lookup-local @13.11-13.17
									(p-assign @12.5-12.11 (ident "result"))))
							(branches
								(branch
									(patterns
										(pattern (degenerate false)
											(p-nominal @14.9-14.18
												(p-applied-tag @14.9-14.18))))
									(value
										(e-call @14.22-14.36
											(e-lookup-external @14.22-14.29
												(module-idx "3")
												(target-node-idx "0"))
											(e-lookup-local @14.30-14.35
												(p-assign @14.12-14.17 (ident "value"))))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-nominal @15.9-15.19
												(p-applied-tag @15.9-15.19))))
									(value
										(e-call @15.23-15.45
											(e-lookup-external @15.23-15.38
												(module-idx "3")
												(target-node-idx "0"))
											(e-lookup-local @15.39-15.44
												(p-assign @15.13-15.18 (ident "error"))))))))))))
		(annotation @11.1-11.14
			(declared-type
				(ty-fn @10.17-10.36 (effectful false)
					(ty-malformed @10.17-10.24)
					(ty-malformed @10.28-10.36)))))
	(d-let
		(p-assign @21.1-21.12 (ident "processData"))
		(e-lambda @21.15-25.6
			(args
				(p-assign @21.16-21.22 (ident "config"))
				(p-assign @21.24-21.30 (ident "values")))
			(e-call @22.5-25.6
				(e-runtime-error (tag "ident_not_in_scope"))
				(e-lookup-local @23.9-23.15
					(p-assign @21.24-21.30 (ident "values")))
				(e-closure @24.9-24.41
					(captures
						(capture @21.16-21.22 (ident "config")))
					(e-lambda @24.9-24.41
						(args
							(p-assign @24.10-24.11 (ident "v")))
						(e-call @24.13-24.41
							(e-lookup-external @24.13-24.30
								(module-idx "2")
								(target-node-idx "0"))
							(e-lookup-local @24.31-24.37
								(p-assign @21.16-21.22 (ident "config")))
							(e-lookup-local @24.39-24.40
								(p-assign @24.10-24.11 (ident "v"))))))))
		(annotation @21.1-21.12
			(declared-type
				(ty-fn @20.15-20.64 (effectful false)
					(ty-malformed @20.15-20.21)
					(ty-apply @20.23-20.34 (name "List") (builtin)
						(ty-malformed @20.28-20.33))
					(ty-apply @20.38-20.64 (name "Result") (local)
						(ty-apply @20.38-20.64 (name "List") (builtin)
							(ty-malformed @20.50-20.55))
						(ty-malformed @20.38-20.64))))))
	(d-let
		(p-assign @36.1-36.13 (ident "createClient"))
		(e-lambda @36.16-36.48
			(args
				(p-assign @36.17-36.23 (ident "config")))
			(e-call @36.25-36.48
				(e-lookup-external @36.25-36.40
					(module-idx "3")
					(target-node-idx "0"))
				(e-lookup-local @36.41-36.47
					(p-assign @36.17-36.23 (ident "config")))))
		(annotation @36.1-36.13
			(declared-type
				(ty-fn @35.16-35.37 (effectful false)
					(ty-malformed @35.16-35.22)
					(ty-lookup @35.26-35.37 (name "Client") (external (module-idx "3") (target-node-idx "0")))))))
	(d-let
		(p-assign @40.1-40.15 (ident "handleResponse"))
		(e-closure @40.18-44.6
			(captures
				(capture @42.12-42.18 (ident "status"))
				(capture @43.13-43.18 (ident "error")))
			(e-lambda @40.18-44.6
				(args
					(p-assign @40.19-40.27 (ident "response")))
				(e-match @41.5-44.6
					(match @41.5-44.6
						(cond
							(e-dot-access @41.11-41.26 (field "status")
								(receiver
									(e-lookup-local @41.11-41.19
										(p-assign @40.19-40.27 (ident "response"))))))
						(branches
							(branch
								(patterns
									(pattern (degenerate false)
										(p-nominal @42.9-42.19
											(p-applied-tag @42.9-42.19))))
								(value
									(e-call @42.23-42.50
										(e-lookup-external @42.23-42.42
											(module-idx "3")
											(target-node-idx "0"))
										(e-lookup-local @42.43-42.49
											(p-assign @42.12-42.18 (ident "status"))))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-nominal @43.9-43.19
											(p-applied-tag @43.9-43.19))))
								(value
									(e-call @43.23-43.44
										(e-runtime-error (tag "ident_not_in_scope"))
										(e-lookup-local @43.38-43.43
											(p-assign @43.13-43.18 (ident "error")))))))))))
		(annotation @40.1-40.15
			(declared-type
				(ty-fn @39.18-39.33 (effectful false)
					(ty-malformed @39.18-39.26)
					(ty-lookup @39.30-39.33 (name "Str") (builtin))))))
	(d-let
		(p-assign @48.1-48.15 (ident "combineResults"))
		(e-closure @48.18-52.6
			(captures
				(capture @51.13-51.18 (ident "error"))
				(capture @50.12-50.17 (ident "value")))
			(e-lambda @48.18-52.6
				(args
					(p-assign @48.19-48.29 (ident "jsonResult"))
					(p-assign @48.31-48.41 (ident "httpStatus")))
				(e-match @49.5-52.6
					(match @49.5-52.6
						(cond
							(e-lookup-local @49.11-49.21
								(p-assign @48.19-48.29 (ident "jsonResult"))))
						(branches
							(branch
								(patterns
									(pattern (degenerate false)
										(p-nominal @50.9-50.18
											(p-applied-tag @50.9-50.18))))
								(value
									(e-nominal @50.22-50.74 (nominal "Result")
										(e-tag @50.22-50.74 (name "Ok")
											(args
												(e-record @50.25-50.73
													(fields
														(field (name "body")
															(e-call @50.33-50.51
																(e-lookup-external @50.33-50.44
																	(module-idx "2")
																	(target-node-idx "0"))
																(e-lookup-local @50.45-50.50
																	(p-assign @50.12-50.17 (ident "value")))))
														(field (name "status")
															(e-lookup-local @50.61-50.71
																(p-assign @48.31-48.41 (ident "httpStatus")))))))))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-nominal @51.9-51.19
											(p-applied-tag @51.9-51.19))))
								(value
									(e-nominal @51.23-51.33 (nominal "Result")
										(e-tag @51.23-51.33 (name "Err")
											(args
												(e-lookup-local @51.27-51.32
													(p-assign @51.13-51.18 (ident "error")))))))))))))
		(annotation @48.1-48.15
			(declared-type
				(ty-fn @47.18-47.73 (effectful false)
					(ty-apply @47.18-47.38 (name "Result") (local)
						(ty-malformed @47.18-47.38)
						(ty-malformed @47.18-47.38))
					(ty-malformed @47.40-47.46)
					(ty-apply @47.50-47.73 (name "Result") (local)
						(ty-malformed @47.50-47.73)
						(ty-malformed @47.50-47.73))))))
	(s-alias-decl @28.1-32.2
		(ty-header @28.1-28.13 (name "ServerConfig"))
		(ty-record @28.16-32.2
			(field (field "jsonConfig")
				(ty-malformed @29.18-29.24))
			(field (field "httpStatus")
				(ty-malformed @30.18-30.24))
			(field (field "defaultResponse")
				(ty-malformed @31.23-31.31))))
	(s-import @1.1-1.49 (module "json.Json") (qualifier "json")
		(exposes
			(exposed (name "Value") (wildcard false))
			(exposed (name "Error") (wildcard false))
			(exposed (name "Config") (wildcard false))))
	(s-import @2.1-2.64 (module "http.Client") (qualifier "http") (alias "Http")
		(exposes
			(exposed (name "Request") (wildcard false))
			(exposed (name "Response") (wildcard false))
			(exposed (name "Status") (wildcard false))))
	(s-import @3.1-3.38 (module "utils.Result") (qualifier "utils")
		(exposes
			(exposed (name "Result") (wildcard false)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @7.1-7.10 (type "Str -> Result(Error, Error)"))
		(patt @11.1-11.14 (type "Error -> Error"))
		(patt @21.1-21.12 (type "Error, List(Error) -> Result(List(Error), Error)"))
		(patt @36.1-36.13 (type "Error -> Error"))
		(patt @40.1-40.15 (type "Error -> Str"))
		(patt @48.1-48.15 (type "Result(Error, Error), Error -> Result(Error, Error)")))
	(type_decls
		(alias @28.1-32.2 (type "ServerConfig")
			(ty-header @28.1-28.13 (name "ServerConfig"))))
	(expressions
		(expr @7.13-7.38 (type "Str -> Result(Error, Error)"))
		(expr @11.17-17.2 (type "Error -> Error"))
		(expr @21.15-25.6 (type "Error, List(Error) -> Result(List(Error), Error)"))
		(expr @36.16-36.48 (type "Error -> Error"))
		(expr @40.18-44.6 (type "Error -> Str"))
		(expr @48.18-52.6 (type "Result(Error, Error), Error -> Result(Error, Error)"))))
~~~

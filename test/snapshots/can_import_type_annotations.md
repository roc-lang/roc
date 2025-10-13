# META
~~~ini
description=Import types and use in type annotations
type=snippet
~~~
# SOURCE
~~~roc
import http.Client as Http exposing [Request, Response]
import json.Json
import utils.Result exposing [Result]

processRequest : Request -> Response
processRequest = |req| Http.defaultResponse

parseJson : Str -> Json.Value
parseJson = |input| Json.parse(input)

handleApi : Http.Request -> Result(Http.Response, Json.Error)
handleApi = |request| {
    result = Json.decode(request.body)
    match result {
        Ok(data) => Ok(Http.success(data))
        Err(err) => Err(err)
    }
}

config : Json.Config
config = Json.defaultConfig

# Test nested type qualification
advancedParser : Json.Parser.Config, Str -> Result(Json.Value, Json.Parser.Error)
advancedParser = |parserConfig, input| Json.Parser.parseWith(parserConfig, input)

# Test function with multiple type parameters
combineResults : Result(a, err), Result(b, err) -> Result((a, b), err)
combineResults = |result1, result2|
    match result1 {
        Ok(value1) =>
            match(result2) {
                Ok(value2) => Ok((value1, value2))
                Err(err) => Err(err)
            }
        Err(err) => Err(err)
    }
~~~
# EXPECTED
MODULE NOT FOUND - can_import_type_annotations.md:1:1:1:56
MODULE NOT FOUND - can_import_type_annotations.md:2:1:2:17
DUPLICATE DEFINITION - can_import_type_annotations.md:1:1:1:1
MODULE NOT FOUND - can_import_type_annotations.md:3:1:3:38
UNDECLARED TYPE - can_import_type_annotations.md:5:18:5:25
UNDECLARED TYPE - can_import_type_annotations.md:5:29:5:37
UNUSED VARIABLE - can_import_type_annotations.md:6:19:6:22
MODULE NOT IMPORTED - can_import_type_annotations.md:24:18:24:36
MODULE NOT IMPORTED - can_import_type_annotations.md:24:64:24:81
# PROBLEMS
**MODULE NOT FOUND**
The module `http.Client` was not found in this Roc project.

You're attempting to use this module here:
**can_import_type_annotations.md:1:1:1:56:**
```roc
import http.Client as Http exposing [Request, Response]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**MODULE NOT FOUND**
The module `json.Json` was not found in this Roc project.

You're attempting to use this module here:
**can_import_type_annotations.md:2:1:2:17:**
```roc
import json.Json
```
^^^^^^^^^^^^^^^^


**DUPLICATE DEFINITION**
The name `Result` is being redeclared in this scope.

The redeclaration is here:
**can_import_type_annotations.md:1:1:1:1:**
```roc
import http.Client as Http exposing [Request, Response]
```
^

But `Result` was already defined here:
**can_import_type_annotations.md:1:1:1:1:**
```roc
import http.Client as Http exposing [Request, Response]
```
^


**MODULE NOT FOUND**
The module `utils.Result` was not found in this Roc project.

You're attempting to use this module here:
**can_import_type_annotations.md:3:1:3:38:**
```roc
import utils.Result exposing [Result]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNDECLARED TYPE**
The type _Request_ is not declared in this scope.

This type is referenced here:
**can_import_type_annotations.md:5:18:5:25:**
```roc
processRequest : Request -> Response
```
                 ^^^^^^^


**UNDECLARED TYPE**
The type _Response_ is not declared in this scope.

This type is referenced here:
**can_import_type_annotations.md:5:29:5:37:**
```roc
processRequest : Request -> Response
```
                            ^^^^^^^^


**UNUSED VARIABLE**
Variable `req` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_req` to suppress this warning.
The unused variable is declared here:
**can_import_type_annotations.md:6:19:6:22:**
```roc
processRequest = |req| Http.defaultResponse
```
                  ^^^


**MODULE NOT IMPORTED**
There is no module with the name `Json.Parser` imported into this Roc file.

You're attempting to use this module here:
**can_import_type_annotations.md:24:18:24:36:**
```roc
advancedParser : Json.Parser.Config, Str -> Result(Json.Value, Json.Parser.Error)
```
                 ^^^^^^^^^^^^^^^^^^


**MODULE NOT IMPORTED**
There is no module with the name `Json.Parser` imported into this Roc file.

You're attempting to use this module here:
**can_import_type_annotations.md:24:64:24:81:**
```roc
advancedParser : Json.Parser.Config, Str -> Result(Json.Value, Json.Parser.Error)
```
                                                               ^^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
KwImport(1:1-1:7),LowerIdent(1:8-1:12),NoSpaceDotUpperIdent(1:12-1:19),KwAs(1:20-1:22),UpperIdent(1:23-1:27),KwExposing(1:28-1:36),OpenSquare(1:37-1:38),UpperIdent(1:38-1:45),Comma(1:45-1:46),UpperIdent(1:47-1:55),CloseSquare(1:55-1:56),
KwImport(2:1-2:7),LowerIdent(2:8-2:12),NoSpaceDotUpperIdent(2:12-2:17),
KwImport(3:1-3:7),LowerIdent(3:8-3:13),NoSpaceDotUpperIdent(3:13-3:20),KwExposing(3:21-3:29),OpenSquare(3:30-3:31),UpperIdent(3:31-3:37),CloseSquare(3:37-3:38),
LowerIdent(5:1-5:15),OpColon(5:16-5:17),UpperIdent(5:18-5:25),OpArrow(5:26-5:28),UpperIdent(5:29-5:37),
LowerIdent(6:1-6:15),OpAssign(6:16-6:17),OpBar(6:18-6:19),LowerIdent(6:19-6:22),OpBar(6:22-6:23),UpperIdent(6:24-6:28),NoSpaceDotLowerIdent(6:28-6:44),
LowerIdent(8:1-8:10),OpColon(8:11-8:12),UpperIdent(8:13-8:16),OpArrow(8:17-8:19),UpperIdent(8:20-8:24),NoSpaceDotUpperIdent(8:24-8:30),
LowerIdent(9:1-9:10),OpAssign(9:11-9:12),OpBar(9:13-9:14),LowerIdent(9:14-9:19),OpBar(9:19-9:20),UpperIdent(9:21-9:25),NoSpaceDotLowerIdent(9:25-9:31),NoSpaceOpenRound(9:31-9:32),LowerIdent(9:32-9:37),CloseRound(9:37-9:38),
LowerIdent(11:1-11:10),OpColon(11:11-11:12),UpperIdent(11:13-11:17),NoSpaceDotUpperIdent(11:17-11:25),OpArrow(11:26-11:28),UpperIdent(11:29-11:35),NoSpaceOpenRound(11:35-11:36),UpperIdent(11:36-11:40),NoSpaceDotUpperIdent(11:40-11:49),Comma(11:49-11:50),UpperIdent(11:51-11:55),NoSpaceDotUpperIdent(11:55-11:61),CloseRound(11:61-11:62),
LowerIdent(12:1-12:10),OpAssign(12:11-12:12),OpBar(12:13-12:14),LowerIdent(12:14-12:21),OpBar(12:21-12:22),OpenCurly(12:23-12:24),
LowerIdent(13:5-13:11),OpAssign(13:12-13:13),UpperIdent(13:14-13:18),NoSpaceDotLowerIdent(13:18-13:25),NoSpaceOpenRound(13:25-13:26),LowerIdent(13:26-13:33),NoSpaceDotLowerIdent(13:33-13:38),CloseRound(13:38-13:39),
KwMatch(14:5-14:10),LowerIdent(14:11-14:17),OpenCurly(14:18-14:19),
UpperIdent(15:9-15:11),NoSpaceOpenRound(15:11-15:12),LowerIdent(15:12-15:16),CloseRound(15:16-15:17),OpFatArrow(15:18-15:20),UpperIdent(15:21-15:23),NoSpaceOpenRound(15:23-15:24),UpperIdent(15:24-15:28),NoSpaceDotLowerIdent(15:28-15:36),NoSpaceOpenRound(15:36-15:37),LowerIdent(15:37-15:41),CloseRound(15:41-15:42),CloseRound(15:42-15:43),
UpperIdent(16:9-16:12),NoSpaceOpenRound(16:12-16:13),LowerIdent(16:13-16:16),CloseRound(16:16-16:17),OpFatArrow(16:18-16:20),UpperIdent(16:21-16:24),NoSpaceOpenRound(16:24-16:25),LowerIdent(16:25-16:28),CloseRound(16:28-16:29),
CloseCurly(17:5-17:6),
CloseCurly(18:1-18:2),
LowerIdent(20:1-20:7),OpColon(20:8-20:9),UpperIdent(20:10-20:14),NoSpaceDotUpperIdent(20:14-20:21),
LowerIdent(21:1-21:7),OpAssign(21:8-21:9),UpperIdent(21:10-21:14),NoSpaceDotLowerIdent(21:14-21:28),
LowerIdent(24:1-24:15),OpColon(24:16-24:17),UpperIdent(24:18-24:22),NoSpaceDotUpperIdent(24:22-24:29),NoSpaceDotUpperIdent(24:29-24:36),Comma(24:36-24:37),UpperIdent(24:38-24:41),OpArrow(24:42-24:44),UpperIdent(24:45-24:51),NoSpaceOpenRound(24:51-24:52),UpperIdent(24:52-24:56),NoSpaceDotUpperIdent(24:56-24:62),Comma(24:62-24:63),UpperIdent(24:64-24:68),NoSpaceDotUpperIdent(24:68-24:75),NoSpaceDotUpperIdent(24:75-24:81),CloseRound(24:81-24:82),
LowerIdent(25:1-25:15),OpAssign(25:16-25:17),OpBar(25:18-25:19),LowerIdent(25:19-25:31),Comma(25:31-25:32),LowerIdent(25:33-25:38),OpBar(25:38-25:39),UpperIdent(25:40-25:44),NoSpaceDotUpperIdent(25:44-25:51),NoSpaceDotLowerIdent(25:51-25:61),NoSpaceOpenRound(25:61-25:62),LowerIdent(25:62-25:74),Comma(25:74-25:75),LowerIdent(25:76-25:81),CloseRound(25:81-25:82),
LowerIdent(28:1-28:15),OpColon(28:16-28:17),UpperIdent(28:18-28:24),NoSpaceOpenRound(28:24-28:25),LowerIdent(28:25-28:26),Comma(28:26-28:27),LowerIdent(28:28-28:31),CloseRound(28:31-28:32),Comma(28:32-28:33),UpperIdent(28:34-28:40),NoSpaceOpenRound(28:40-28:41),LowerIdent(28:41-28:42),Comma(28:42-28:43),LowerIdent(28:44-28:47),CloseRound(28:47-28:48),OpArrow(28:49-28:51),UpperIdent(28:52-28:58),NoSpaceOpenRound(28:58-28:59),NoSpaceOpenRound(28:59-28:60),LowerIdent(28:60-28:61),Comma(28:61-28:62),LowerIdent(28:63-28:64),CloseRound(28:64-28:65),Comma(28:65-28:66),LowerIdent(28:67-28:70),CloseRound(28:70-28:71),
LowerIdent(29:1-29:15),OpAssign(29:16-29:17),OpBar(29:18-29:19),LowerIdent(29:19-29:26),Comma(29:26-29:27),LowerIdent(29:28-29:35),OpBar(29:35-29:36),
KwMatch(30:5-30:10),LowerIdent(30:11-30:18),OpenCurly(30:19-30:20),
UpperIdent(31:9-31:11),NoSpaceOpenRound(31:11-31:12),LowerIdent(31:12-31:18),CloseRound(31:18-31:19),OpFatArrow(31:20-31:22),
KwMatch(32:13-32:18),NoSpaceOpenRound(32:18-32:19),LowerIdent(32:19-32:26),CloseRound(32:26-32:27),OpenCurly(32:28-32:29),
UpperIdent(33:17-33:19),NoSpaceOpenRound(33:19-33:20),LowerIdent(33:20-33:26),CloseRound(33:26-33:27),OpFatArrow(33:28-33:30),UpperIdent(33:31-33:33),NoSpaceOpenRound(33:33-33:34),NoSpaceOpenRound(33:34-33:35),LowerIdent(33:35-33:41),Comma(33:41-33:42),LowerIdent(33:43-33:49),CloseRound(33:49-33:50),CloseRound(33:50-33:51),
UpperIdent(34:17-34:20),NoSpaceOpenRound(34:20-34:21),LowerIdent(34:21-34:24),CloseRound(34:24-34:25),OpFatArrow(34:26-34:28),UpperIdent(34:29-34:32),NoSpaceOpenRound(34:32-34:33),LowerIdent(34:33-34:36),CloseRound(34:36-34:37),
CloseCurly(35:13-35:14),
UpperIdent(36:9-36:12),NoSpaceOpenRound(36:12-36:13),LowerIdent(36:13-36:16),CloseRound(36:16-36:17),OpFatArrow(36:18-36:20),UpperIdent(36:21-36:24),NoSpaceOpenRound(36:24-36:25),LowerIdent(36:25-36:28),CloseRound(36:28-36:29),
CloseCurly(37:5-37:6),
EndOfFile(38:1-38:1),
~~~
# PARSE
~~~clojure
(file @1.1-37.6
	(type-module @1.1-1.7)
	(statements
		(s-import @1.1-1.56 (raw "http.Client") (alias "Http")
			(exposing
				(exposed-upper-ident @1.38-1.45 (text "Request"))
				(exposed-upper-ident @1.47-1.55 (text "Response"))))
		(s-import @2.1-2.17 (raw "json.Json"))
		(s-import @3.1-3.38 (raw "utils.Result")
			(exposing
				(exposed-upper-ident @3.31-3.37 (text "Result"))))
		(s-type-anno @5.1-5.37 (name "processRequest")
			(ty-fn @5.18-5.37
				(ty @5.18-5.25 (name "Request"))
				(ty @5.29-5.37 (name "Response"))))
		(s-decl @6.1-6.44
			(p-ident @6.1-6.15 (raw "processRequest"))
			(e-lambda @6.18-6.44
				(args
					(p-ident @6.19-6.22 (raw "req")))
				(e-ident @6.24-6.44 (raw "Http.defaultResponse"))))
		(s-type-anno @8.1-8.30 (name "parseJson")
			(ty-fn @8.13-8.30
				(ty @8.13-8.16 (name "Str"))
				(ty @8.20-8.30 (name "Json.Value"))))
		(s-decl @9.1-9.38
			(p-ident @9.1-9.10 (raw "parseJson"))
			(e-lambda @9.13-9.38
				(args
					(p-ident @9.14-9.19 (raw "input")))
				(e-apply @9.21-9.38
					(e-ident @9.21-9.31 (raw "Json.parse"))
					(e-ident @9.32-9.37 (raw "input")))))
		(s-type-anno @11.1-11.62 (name "handleApi")
			(ty-fn @11.13-11.62
				(ty @11.13-11.25 (name "Http.Request"))
				(ty-apply @11.29-11.62
					(ty @11.29-11.35 (name "Result"))
					(ty @11.36-11.49 (name "Http.Response"))
					(ty @11.51-11.61 (name "Json.Error")))))
		(s-decl @12.1-18.2
			(p-ident @12.1-12.10 (raw "handleApi"))
			(e-lambda @12.13-18.2
				(args
					(p-ident @12.14-12.21 (raw "request")))
				(e-block @12.23-18.2
					(statements
						(s-decl @13.5-13.39
							(p-ident @13.5-13.11 (raw "result"))
							(e-apply @13.14-13.39
								(e-ident @13.14-13.25 (raw "Json.decode"))
								(e-field-access @13.26-13.38
									(e-ident @13.26-13.33 (raw "request"))
									(e-ident @13.33-13.38 (raw "body")))))
						(e-match
							(e-ident @14.11-14.17 (raw "result"))
							(branches
								(branch @15.9-15.43
									(p-tag @15.9-15.17 (raw "Ok")
										(p-ident @15.12-15.16 (raw "data")))
									(e-apply @15.21-15.43
										(e-tag @15.21-15.23 (raw "Ok"))
										(e-apply @15.24-15.42
											(e-ident @15.24-15.36 (raw "Http.success"))
											(e-ident @15.37-15.41 (raw "data")))))
								(branch @16.9-16.29
									(p-tag @16.9-16.17 (raw "Err")
										(p-ident @16.13-16.16 (raw "err")))
									(e-apply @16.21-16.29
										(e-tag @16.21-16.24 (raw "Err"))
										(e-ident @16.25-16.28 (raw "err"))))))))))
		(s-type-anno @20.1-20.21 (name "config")
			(ty @20.10-20.21 (name "Json.Config")))
		(s-decl @21.1-21.28
			(p-ident @21.1-21.7 (raw "config"))
			(e-ident @21.10-21.28 (raw "Json.defaultConfig")))
		(s-type-anno @24.1-24.82 (name "advancedParser")
			(ty-fn @24.18-24.82
				(ty @24.18-24.36 (name "Json.Parser.Config"))
				(ty @24.38-24.41 (name "Str"))
				(ty-apply @24.45-24.82
					(ty @24.45-24.51 (name "Result"))
					(ty @24.52-24.62 (name "Json.Value"))
					(ty @24.64-24.81 (name "Json.Parser.Error")))))
		(s-decl @25.1-25.82
			(p-ident @25.1-25.15 (raw "advancedParser"))
			(e-lambda @25.18-25.82
				(args
					(p-ident @25.19-25.31 (raw "parserConfig"))
					(p-ident @25.33-25.38 (raw "input")))
				(e-apply @25.40-25.82
					(e-ident @25.40-25.61 (raw "Json.Parser.parseWith"))
					(e-ident @25.62-25.74 (raw "parserConfig"))
					(e-ident @25.76-25.81 (raw "input")))))
		(s-type-anno @28.1-28.71 (name "combineResults")
			(ty-fn @28.18-28.71
				(ty-apply @28.18-28.32
					(ty @28.18-28.24 (name "Result"))
					(ty-var @28.25-28.26 (raw "a"))
					(ty-var @28.28-28.31 (raw "err")))
				(ty-apply @28.34-28.48
					(ty @28.34-28.40 (name "Result"))
					(ty-var @28.41-28.42 (raw "b"))
					(ty-var @28.44-28.47 (raw "err")))
				(ty-apply @28.52-28.71
					(ty @28.52-28.58 (name "Result"))
					(ty-tuple @28.59-28.65
						(ty-var @28.60-28.61 (raw "a"))
						(ty-var @28.63-28.64 (raw "b")))
					(ty-var @28.67-28.70 (raw "err")))))
		(s-decl @29.1-37.6
			(p-ident @29.1-29.15 (raw "combineResults"))
			(e-lambda @29.18-37.6
				(args
					(p-ident @29.19-29.26 (raw "result1"))
					(p-ident @29.28-29.35 (raw "result2")))
				(e-match
					(e-ident @30.11-30.18 (raw "result1"))
					(branches
						(branch @31.9-35.14
							(p-tag @31.9-31.19 (raw "Ok")
								(p-ident @31.12-31.18 (raw "value1")))
							(e-match
								(e-tuple @32.18-32.27
									(e-ident @32.19-32.26 (raw "result2")))
								(branches
									(branch @33.17-33.51
										(p-tag @33.17-33.27 (raw "Ok")
											(p-ident @33.20-33.26 (raw "value2")))
										(e-apply @33.31-33.51
											(e-tag @33.31-33.33 (raw "Ok"))
											(e-tuple @33.34-33.50
												(e-ident @33.35-33.41 (raw "value1"))
												(e-ident @33.43-33.49 (raw "value2")))))
									(branch @34.17-34.37
										(p-tag @34.17-34.25 (raw "Err")
											(p-ident @34.21-34.24 (raw "err")))
										(e-apply @34.29-34.37
											(e-tag @34.29-34.32 (raw "Err"))
											(e-ident @34.33-34.36 (raw "err")))))))
						(branch @36.9-36.29
							(p-tag @36.9-36.17 (raw "Err")
								(p-ident @36.13-36.16 (raw "err")))
							(e-apply @36.21-36.29
								(e-tag @36.21-36.24 (raw "Err"))
								(e-ident @36.25-36.28 (raw "err"))))))))))
~~~
# FORMATTED
~~~roc
import http.Client as Http exposing [Request, Response]
import json.Json
import utils.Result exposing [Result]

processRequest : Request -> Response
processRequest = |req| Http.defaultResponse

parseJson : Str -> Json.Value
parseJson = |input| Json.parse(input)

handleApi : Http.Request -> Result(Http.Response, Json.Error)
handleApi = |request| {
	result = Json.decode(request.body)
	match result {
		Ok(data) => Ok(Http.success(data))
		Err(err) => Err(err)
	}
}

config : Json.Config
config = Json.defaultConfig

# Test nested type qualification
advancedParser : Json.Parser.Config, Str -> Result(Json.Value, Json.Parser.Error)
advancedParser = |parserConfig, input| Json.Parser.parseWith(parserConfig, input)

# Test function with multiple type parameters
combineResults : Result(a, err), Result(b, err) -> Result((a, b), err)
combineResults = |result1, result2|
	match result1 {
		Ok(value1) =>
			match (result2) {
				Ok(value2) => Ok((value1, value2))
				Err(err) => Err(err)
			}
		Err(err) => Err(err)
	}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @6.1-6.15 (ident "processRequest"))
		(e-lambda @6.18-6.44
			(args
				(p-assign @6.19-6.22 (ident "req")))
			(e-lookup-external @6.24-6.44
				(module-idx "4")
				(target-node-idx "0")))
		(annotation @6.1-6.15
			(declared-type
				(ty-fn @5.18-5.37 (effectful false)
					(ty-malformed @5.18-5.25)
					(ty-malformed @5.29-5.37)))))
	(d-let
		(p-assign @9.1-9.10 (ident "parseJson"))
		(e-lambda @9.13-9.38
			(args
				(p-assign @9.14-9.19 (ident "input")))
			(e-call @9.21-9.38
				(e-lookup-external @9.21-9.31
					(module-idx "5")
					(target-node-idx "0"))
				(e-lookup-local @9.32-9.37
					(p-assign @9.14-9.19 (ident "input")))))
		(annotation @9.1-9.10
			(declared-type
				(ty-fn @8.13-8.30 (effectful false)
					(ty-lookup @8.13-8.16 (name "Str") (builtin))
					(ty-lookup @8.20-8.30 (name "Value") (external (module-idx "5") (target-node-idx "0")))))))
	(d-let
		(p-assign @12.1-12.10 (ident "handleApi"))
		(e-closure @12.13-18.2
			(captures
				(capture @15.12-15.16 (ident "data"))
				(capture @16.13-16.16 (ident "err")))
			(e-lambda @12.13-18.2
				(args
					(p-assign @12.14-12.21 (ident "request")))
				(e-block @12.23-18.2
					(s-let @13.5-13.39
						(p-assign @13.5-13.11 (ident "result"))
						(e-call @13.14-13.39
							(e-lookup-external @13.14-13.25
								(module-idx "5")
								(target-node-idx "0"))
							(e-dot-access @13.26-13.38 (field "body")
								(receiver
									(e-lookup-local @13.26-13.33
										(p-assign @12.14-12.21 (ident "request")))))))
					(e-match @14.5-17.6
						(match @14.5-17.6
							(cond
								(e-lookup-local @14.11-14.17
									(p-assign @13.5-13.11 (ident "result"))))
							(branches
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag @15.9-15.17)))
									(value
										(e-tag @15.21-15.43 (name "Ok")
											(args
												(e-call @15.24-15.42
													(e-lookup-external @15.24-15.36
														(module-idx "4")
														(target-node-idx "0"))
													(e-lookup-local @15.37-15.41
														(p-assign @15.12-15.16 (ident "data"))))))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag @16.9-16.17)))
									(value
										(e-tag @16.21-16.29 (name "Err")
											(args
												(e-lookup-local @16.25-16.28
													(p-assign @16.13-16.16 (ident "err")))))))))))))
		(annotation @12.1-12.10
			(declared-type
				(ty-fn @11.13-11.62 (effectful false)
					(ty-lookup @11.13-11.25 (name "Request") (external (module-idx "4") (target-node-idx "0")))
					(ty-apply @11.29-11.62 (name "Result") (external (module-idx "3") (target-node-idx "3"))
						(ty-lookup @11.36-11.49 (name "Response") (external (module-idx "4") (target-node-idx "0")))
						(ty-lookup @11.51-11.61 (name "Error") (external (module-idx "5") (target-node-idx "0"))))))))
	(d-let
		(p-assign @21.1-21.7 (ident "config"))
		(e-lookup-external @21.10-21.28
			(module-idx "5")
			(target-node-idx "0"))
		(annotation @21.1-21.7
			(declared-type
				(ty-lookup @20.10-20.21 (name "Config") (external (module-idx "5") (target-node-idx "0"))))))
	(d-let
		(p-assign @25.1-25.15 (ident "advancedParser"))
		(e-lambda @25.18-25.82
			(args
				(p-assign @25.19-25.31 (ident "parserConfig"))
				(p-assign @25.33-25.38 (ident "input")))
			(e-call @25.40-25.82
				(e-lookup-external @25.40-25.61
					(module-idx "5")
					(target-node-idx "0"))
				(e-lookup-local @25.62-25.74
					(p-assign @25.19-25.31 (ident "parserConfig")))
				(e-lookup-local @25.76-25.81
					(p-assign @25.33-25.38 (ident "input")))))
		(annotation @25.1-25.15
			(declared-type
				(ty-fn @24.18-24.82 (effectful false)
					(ty-malformed @24.18-24.36)
					(ty-lookup @24.38-24.41 (name "Str") (builtin))
					(ty-apply @24.45-24.82 (name "Result") (external (module-idx "3") (target-node-idx "3"))
						(ty-lookup @24.52-24.62 (name "Value") (external (module-idx "5") (target-node-idx "0")))
						(ty-malformed @24.64-24.81))))))
	(d-let
		(p-assign @29.1-29.15 (ident "combineResults"))
		(e-closure @29.18-37.6
			(captures
				(capture @31.12-31.18 (ident "value1"))
				(capture @36.13-36.16 (ident "err"))
				(capture @33.20-33.26 (ident "value2"))
				(capture @34.21-34.24 (ident "err")))
			(e-lambda @29.18-37.6
				(args
					(p-assign @29.19-29.26 (ident "result1"))
					(p-assign @29.28-29.35 (ident "result2")))
				(e-match @30.5-37.6
					(match @30.5-37.6
						(cond
							(e-lookup-local @30.11-30.18
								(p-assign @29.19-29.26 (ident "result1"))))
						(branches
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag @31.9-31.19)))
								(value
									(e-match @32.13-35.14
										(match @32.13-35.14
											(cond
												(e-lookup-local @32.19-32.26
													(p-assign @29.28-29.35 (ident "result2"))))
											(branches
												(branch
													(patterns
														(pattern (degenerate false)
															(p-applied-tag @33.17-33.27)))
													(value
														(e-tag @33.31-33.51 (name "Ok")
															(args
																(e-tuple @33.34-33.50
																	(elems
																		(e-lookup-local @33.35-33.41
																			(p-assign @31.12-31.18 (ident "value1")))
																		(e-lookup-local @33.43-33.49
																			(p-assign @33.20-33.26 (ident "value2")))))))))
												(branch
													(patterns
														(pattern (degenerate false)
															(p-applied-tag @34.17-34.25)))
													(value
														(e-tag @34.29-34.37 (name "Err")
															(args
																(e-lookup-local @34.33-34.36
																	(p-assign @34.21-34.24 (ident "err"))))))))))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag @36.9-36.17)))
								(value
									(e-tag @36.21-36.29 (name "Err")
										(args
											(e-lookup-local @36.25-36.28
												(p-assign @36.13-36.16 (ident "err"))))))))))))
		(annotation @29.1-29.15
			(declared-type
				(ty-fn @28.18-28.71 (effectful false)
					(ty-apply @28.18-28.32 (name "Result") (external (module-idx "3") (target-node-idx "3"))
						(ty-rigid-var @28.25-28.26 (name "a"))
						(ty-rigid-var @28.28-28.31 (name "err")))
					(ty-apply @28.34-28.48 (name "Result") (external (module-idx "3") (target-node-idx "3"))
						(ty-rigid-var @28.41-28.42 (name "b"))
						(ty-rigid-var-lookup (ty-rigid-var @28.28-28.31 (name "err"))))
					(ty-apply @28.52-28.71 (name "Result") (external (module-idx "3") (target-node-idx "3"))
						(ty-tuple @28.59-28.65
							(ty-rigid-var-lookup (ty-rigid-var @28.25-28.26 (name "a")))
							(ty-rigid-var-lookup (ty-rigid-var @28.41-28.42 (name "b"))))
						(ty-rigid-var-lookup (ty-rigid-var @28.28-28.31 (name "err"))))))))
	(s-import @1.1-1.56 (module "http.Client")
		(exposes
			(exposed (name "Request") (wildcard false))
			(exposed (name "Response") (wildcard false))))
	(s-import @2.1-2.17 (module "json.Json")
		(exposes))
	(s-import @3.1-3.38 (module "utils.Result")
		(exposes
			(exposed (name "Result") (wildcard false)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.15 (type "Error -> Error"))
		(patt @9.1-9.10 (type "Str -> Error"))
		(patt @12.1-12.10 (type "Error -> Error"))
		(patt @21.1-21.7 (type "Error"))
		(patt @25.1-25.15 (type "Error, Str -> Error"))
		(patt @29.1-29.15 (type "Error, Error -> Error")))
	(expressions
		(expr @6.18-6.44 (type "Error -> Error"))
		(expr @9.13-9.38 (type "Str -> Error"))
		(expr @12.13-18.2 (type "Error -> Error"))
		(expr @21.10-21.28 (type "Error"))
		(expr @25.18-25.82 (type "Error, Str -> Error"))
		(expr @29.18-37.6 (type "Error, Error -> Error"))))
~~~

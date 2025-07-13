# META
~~~ini
description=Import types and use in type annotations
type=file
~~~
# SOURCE
~~~roc
module []

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
UNDECLARED TYPE - can_import_type_annotations.md:7:18:7:25
UNDECLARED TYPE - can_import_type_annotations.md:7:29:7:37
UNUSED VARIABLE - can_import_type_annotations.md:8:19:8:22
# PROBLEMS
**UNDECLARED TYPE**
The type ``Request`` is not declared in this scope.

This type is referenced here:
**can_import_type_annotations.md:7:18:7:25:**
```roc
processRequest : Request -> Response
```
                 ^^^^^^^


**UNDECLARED TYPE**
The type ``Response`` is not declared in this scope.

This type is referenced here:
**can_import_type_annotations.md:7:29:7:37:**
```roc
processRequest : Request -> Response
```
                            ^^^^^^^^


**UNUSED VARIABLE**
Variable ``req`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_req` to suppress this warning.
The unused variable is declared here:
**can_import_type_annotations.md:8:19:8:22:**
```roc
processRequest = |req| Http.defaultResponse
```
                  ^^^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
KwImport(3:1-3:7),LowerIdent(3:8-3:12),NoSpaceDotUpperIdent(3:12-3:19),KwAs(3:20-3:22),UpperIdent(3:23-3:27),KwExposing(3:28-3:36),OpenSquare(3:37-3:38),UpperIdent(3:38-3:45),Comma(3:45-3:46),UpperIdent(3:47-3:55),CloseSquare(3:55-3:56),
KwImport(4:1-4:7),LowerIdent(4:8-4:12),NoSpaceDotUpperIdent(4:12-4:17),
KwImport(5:1-5:7),LowerIdent(5:8-5:13),NoSpaceDotUpperIdent(5:13-5:20),KwExposing(5:21-5:29),OpenSquare(5:30-5:31),UpperIdent(5:31-5:37),CloseSquare(5:37-5:38),
LowerIdent(7:1-7:15),OpColon(7:16-7:17),UpperIdent(7:18-7:25),OpArrow(7:26-7:28),UpperIdent(7:29-7:37),
LowerIdent(8:1-8:15),OpAssign(8:16-8:17),OpBar(8:18-8:19),LowerIdent(8:19-8:22),OpBar(8:22-8:23),UpperIdent(8:24-8:28),NoSpaceDotLowerIdent(8:28-8:44),
LowerIdent(10:1-10:10),OpColon(10:11-10:12),UpperIdent(10:13-10:16),OpArrow(10:17-10:19),UpperIdent(10:20-10:24),NoSpaceDotUpperIdent(10:24-10:30),
LowerIdent(11:1-11:10),OpAssign(11:11-11:12),OpBar(11:13-11:14),LowerIdent(11:14-11:19),OpBar(11:19-11:20),UpperIdent(11:21-11:25),NoSpaceDotLowerIdent(11:25-11:31),NoSpaceOpenRound(11:31-11:32),LowerIdent(11:32-11:37),CloseRound(11:37-11:38),
LowerIdent(13:1-13:10),OpColon(13:11-13:12),UpperIdent(13:13-13:17),NoSpaceDotUpperIdent(13:17-13:25),OpArrow(13:26-13:28),UpperIdent(13:29-13:35),NoSpaceOpenRound(13:35-13:36),UpperIdent(13:36-13:40),NoSpaceDotUpperIdent(13:40-13:49),Comma(13:49-13:50),UpperIdent(13:51-13:55),NoSpaceDotUpperIdent(13:55-13:61),CloseRound(13:61-13:62),
LowerIdent(14:1-14:10),OpAssign(14:11-14:12),OpBar(14:13-14:14),LowerIdent(14:14-14:21),OpBar(14:21-14:22),OpenCurly(14:23-14:24),
LowerIdent(15:5-15:11),OpAssign(15:12-15:13),UpperIdent(15:14-15:18),NoSpaceDotLowerIdent(15:18-15:25),NoSpaceOpenRound(15:25-15:26),LowerIdent(15:26-15:33),NoSpaceDotLowerIdent(15:33-15:38),CloseRound(15:38-15:39),
KwMatch(16:5-16:10),LowerIdent(16:11-16:17),OpenCurly(16:18-16:19),
UpperIdent(17:9-17:11),NoSpaceOpenRound(17:11-17:12),LowerIdent(17:12-17:16),CloseRound(17:16-17:17),OpFatArrow(17:18-17:20),UpperIdent(17:21-17:23),NoSpaceOpenRound(17:23-17:24),UpperIdent(17:24-17:28),NoSpaceDotLowerIdent(17:28-17:36),NoSpaceOpenRound(17:36-17:37),LowerIdent(17:37-17:41),CloseRound(17:41-17:42),CloseRound(17:42-17:43),
UpperIdent(18:9-18:12),NoSpaceOpenRound(18:12-18:13),LowerIdent(18:13-18:16),CloseRound(18:16-18:17),OpFatArrow(18:18-18:20),UpperIdent(18:21-18:24),NoSpaceOpenRound(18:24-18:25),LowerIdent(18:25-18:28),CloseRound(18:28-18:29),
CloseCurly(19:5-19:6),
CloseCurly(20:1-20:2),
LowerIdent(22:1-22:7),OpColon(22:8-22:9),UpperIdent(22:10-22:14),NoSpaceDotUpperIdent(22:14-22:21),
LowerIdent(23:1-23:7),OpAssign(23:8-23:9),UpperIdent(23:10-23:14),NoSpaceDotLowerIdent(23:14-23:28),
LowerIdent(26:1-26:15),OpColon(26:16-26:17),UpperIdent(26:18-26:22),NoSpaceDotUpperIdent(26:22-26:29),NoSpaceDotUpperIdent(26:29-26:36),Comma(26:36-26:37),UpperIdent(26:38-26:41),OpArrow(26:42-26:44),UpperIdent(26:45-26:51),NoSpaceOpenRound(26:51-26:52),UpperIdent(26:52-26:56),NoSpaceDotUpperIdent(26:56-26:62),Comma(26:62-26:63),UpperIdent(26:64-26:68),NoSpaceDotUpperIdent(26:68-26:75),NoSpaceDotUpperIdent(26:75-26:81),CloseRound(26:81-26:82),
LowerIdent(27:1-27:15),OpAssign(27:16-27:17),OpBar(27:18-27:19),LowerIdent(27:19-27:31),Comma(27:31-27:32),LowerIdent(27:33-27:38),OpBar(27:38-27:39),UpperIdent(27:40-27:44),NoSpaceDotUpperIdent(27:44-27:51),NoSpaceDotLowerIdent(27:51-27:61),NoSpaceOpenRound(27:61-27:62),LowerIdent(27:62-27:74),Comma(27:74-27:75),LowerIdent(27:76-27:81),CloseRound(27:81-27:82),
LowerIdent(30:1-30:15),OpColon(30:16-30:17),UpperIdent(30:18-30:24),NoSpaceOpenRound(30:24-30:25),LowerIdent(30:25-30:26),Comma(30:26-30:27),LowerIdent(30:28-30:31),CloseRound(30:31-30:32),Comma(30:32-30:33),UpperIdent(30:34-30:40),NoSpaceOpenRound(30:40-30:41),LowerIdent(30:41-30:42),Comma(30:42-30:43),LowerIdent(30:44-30:47),CloseRound(30:47-30:48),OpArrow(30:49-30:51),UpperIdent(30:52-30:58),NoSpaceOpenRound(30:58-30:59),NoSpaceOpenRound(30:59-30:60),LowerIdent(30:60-30:61),Comma(30:61-30:62),LowerIdent(30:63-30:64),CloseRound(30:64-30:65),Comma(30:65-30:66),LowerIdent(30:67-30:70),CloseRound(30:70-30:71),
LowerIdent(31:1-31:15),OpAssign(31:16-31:17),OpBar(31:18-31:19),LowerIdent(31:19-31:26),Comma(31:26-31:27),LowerIdent(31:28-31:35),OpBar(31:35-31:36),
KwMatch(32:5-32:10),LowerIdent(32:11-32:18),OpenCurly(32:19-32:20),
UpperIdent(33:9-33:11),NoSpaceOpenRound(33:11-33:12),LowerIdent(33:12-33:18),CloseRound(33:18-33:19),OpFatArrow(33:20-33:22),
KwMatch(34:13-34:18),NoSpaceOpenRound(34:18-34:19),LowerIdent(34:19-34:26),CloseRound(34:26-34:27),OpenCurly(34:28-34:29),
UpperIdent(35:17-35:19),NoSpaceOpenRound(35:19-35:20),LowerIdent(35:20-35:26),CloseRound(35:26-35:27),OpFatArrow(35:28-35:30),UpperIdent(35:31-35:33),NoSpaceOpenRound(35:33-35:34),NoSpaceOpenRound(35:34-35:35),LowerIdent(35:35-35:41),Comma(35:41-35:42),LowerIdent(35:43-35:49),CloseRound(35:49-35:50),CloseRound(35:50-35:51),
UpperIdent(36:17-36:20),NoSpaceOpenRound(36:20-36:21),LowerIdent(36:21-36:24),CloseRound(36:24-36:25),OpFatArrow(36:26-36:28),UpperIdent(36:29-36:32),NoSpaceOpenRound(36:32-36:33),LowerIdent(36:33-36:36),CloseRound(36:36-36:37),
CloseCurly(37:13-37:14),
UpperIdent(38:9-38:12),NoSpaceOpenRound(38:12-38:13),LowerIdent(38:13-38:16),CloseRound(38:16-38:17),OpFatArrow(38:18-38:20),UpperIdent(38:21-38:24),NoSpaceOpenRound(38:24-38:25),LowerIdent(38:25-38:28),CloseRound(38:28-38:29),
CloseCurly(39:5-39:6),EndOfFile(39:6-39:6),
~~~
# PARSE
~~~clojure
(file @1.1-39.6
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-import @3.1-3.56 (raw "http.Client") (alias "Http")
			(exposing
				(exposed-upper-ident @3.38-3.45 (text "Request"))
				(exposed-upper-ident @3.47-3.55 (text "Response"))))
		(s-import @4.1-4.17 (raw "json.Json"))
		(s-import @5.1-5.38 (raw "utils.Result")
			(exposing
				(exposed-upper-ident @5.31-5.37 (text "Result"))))
		(s-type-anno @7.1-7.37 (name "processRequest")
			(ty-fn @7.18-7.37
				(ty @7.18-7.25 (name "Request"))
				(ty @7.29-7.37 (name "Response"))))
		(s-decl @8.1-8.44
			(p-ident @8.1-8.15 (raw "processRequest"))
			(e-lambda @8.18-8.44
				(args
					(p-ident @8.19-8.22 (raw "req")))
				(e-ident @8.24-8.44 (raw "Http.defaultResponse"))))
		(s-type-anno @10.1-10.30 (name "parseJson")
			(ty-fn @10.13-10.30
				(ty @10.13-10.16 (name "Str"))
				(ty @10.20-10.30 (name "Json.Value"))))
		(s-decl @11.1-11.38
			(p-ident @11.1-11.10 (raw "parseJson"))
			(e-lambda @11.13-11.38
				(args
					(p-ident @11.14-11.19 (raw "input")))
				(e-apply @11.21-11.38
					(e-ident @11.21-11.31 (raw "Json.parse"))
					(e-ident @11.32-11.37 (raw "input")))))
		(s-type-anno @13.1-13.62 (name "handleApi")
			(ty-fn @13.13-13.62
				(ty @13.13-13.25 (name "Http.Request"))
				(ty-apply @13.29-13.62
					(ty @13.29-13.35 (name "Result"))
					(ty @13.36-13.49 (name "Http.Response"))
					(ty @13.51-13.61 (name "Json.Error")))))
		(s-decl @14.1-20.2
			(p-ident @14.1-14.10 (raw "handleApi"))
			(e-lambda @14.13-20.2
				(args
					(p-ident @14.14-14.21 (raw "request")))
				(e-block @14.23-20.2
					(statements
						(s-decl @15.5-15.39
							(p-ident @15.5-15.11 (raw "result"))
							(e-apply @15.14-15.39
								(e-ident @15.14-15.25 (raw "Json.decode"))
								(e-field-access @15.26-15.38
									(e-ident @15.26-15.33 (raw "request"))
									(e-ident @15.33-15.38 (raw "body")))))
						(e-match
							(e-ident @16.11-16.17 (raw "result"))
							(branches
								(branch @17.9-17.43
									(p-tag @17.9-17.17 (raw "Ok")
										(p-ident @17.12-17.16 (raw "data")))
									(e-apply @17.21-17.43
										(e-tag @17.21-17.23 (raw "Ok"))
										(e-apply @17.24-17.42
											(e-ident @17.24-17.36 (raw "Http.success"))
											(e-ident @17.37-17.41 (raw "data")))))
								(branch @18.9-18.29
									(p-tag @18.9-18.17 (raw "Err")
										(p-ident @18.13-18.16 (raw "err")))
									(e-apply @18.21-18.29
										(e-tag @18.21-18.24 (raw "Err"))
										(e-ident @18.25-18.28 (raw "err"))))))))))
		(s-type-anno @22.1-22.21 (name "config")
			(ty @22.10-22.21 (name "Json.Config")))
		(s-decl @23.1-23.28
			(p-ident @23.1-23.7 (raw "config"))
			(e-ident @23.10-23.28 (raw "Json.defaultConfig")))
		(s-type-anno @26.1-26.82 (name "advancedParser")
			(ty-fn @26.18-26.82
				(ty @26.18-26.36 (name "Json.Parser.Config"))
				(ty @26.38-26.41 (name "Str"))
				(ty-apply @26.45-26.82
					(ty @26.45-26.51 (name "Result"))
					(ty @26.52-26.62 (name "Json.Value"))
					(ty @26.64-26.81 (name "Json.Parser.Error")))))
		(s-decl @27.1-27.82
			(p-ident @27.1-27.15 (raw "advancedParser"))
			(e-lambda @27.18-27.82
				(args
					(p-ident @27.19-27.31 (raw "parserConfig"))
					(p-ident @27.33-27.38 (raw "input")))
				(e-apply @27.40-27.82
					(e-ident @27.40-27.61 (raw "Json.Parser.parseWith"))
					(e-ident @27.62-27.74 (raw "parserConfig"))
					(e-ident @27.76-27.81 (raw "input")))))
		(s-type-anno @30.1-30.71 (name "combineResults")
			(ty-fn @30.18-30.71
				(ty-apply @30.18-30.32
					(ty @30.18-30.24 (name "Result"))
					(ty-var @30.25-30.25 (raw "a"))
					(ty-var @1.1-1.1 (raw "err")))
				(ty-apply @30.34-30.48
					(ty @30.34-30.40 (name "Result"))
					(ty-var @30.41-30.41 (raw "b"))
					(ty-var @1.1-1.1 (raw "err")))
				(ty-apply @30.52-30.71
					(ty @30.52-30.58 (name "Result"))
					(ty-tuple @30.59-30.65
						(ty-var @30.60-30.60 (raw "a"))
						(ty-var @1.1-1.1 (raw "b")))
					(ty-var @1.1-1.1 (raw "err")))))
		(s-decl @31.1-39.6
			(p-ident @31.1-31.15 (raw "combineResults"))
			(e-lambda @31.18-39.6
				(args
					(p-ident @31.19-31.26 (raw "result1"))
					(p-ident @31.28-31.35 (raw "result2")))
				(e-match
					(e-ident @32.11-32.18 (raw "result1"))
					(branches
						(branch @33.9-37.14
							(p-tag @33.9-33.19 (raw "Ok")
								(p-ident @33.12-33.18 (raw "value1")))
							(e-match
								(e-tuple @34.18-34.27
									(e-ident @34.19-34.26 (raw "result2")))
								(branches
									(branch @35.17-35.51
										(p-tag @35.17-35.27 (raw "Ok")
											(p-ident @35.20-35.26 (raw "value2")))
										(e-apply @35.31-35.51
											(e-tag @35.31-35.33 (raw "Ok"))
											(e-tuple @35.34-35.50
												(e-ident @35.35-35.41 (raw "value1"))
												(e-ident @35.43-35.49 (raw "value2")))))
									(branch @36.17-36.37
										(p-tag @36.17-36.25 (raw "Err")
											(p-ident @36.21-36.24 (raw "err")))
										(e-apply @36.29-36.37
											(e-tag @36.29-36.32 (raw "Err"))
											(e-ident @36.33-36.36 (raw "err")))))))
						(branch @38.9-38.29
							(p-tag @38.9-38.17 (raw "Err")
								(p-ident @38.13-38.16 (raw "err")))
							(e-apply @38.21-38.29
								(e-tag @38.21-38.24 (raw "Err"))
								(e-ident @38.25-38.28 (raw "err"))))))))))
~~~
# FORMATTED
~~~roc
module []

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
advancedParser = |parserConfig, input| Json.parseWith(parserConfig, input)

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
		(p-assign @8.1-8.15 (ident "processRequest"))
		(e-lambda @8.18-8.44
			(args
				(p-assign @8.19-8.22 (ident "req")))
			(e-lookup-external @8.24-8.44
				(module-idx "0")
				(target-node-idx "0")))
		(annotation @8.1-8.15
			(declared-type
				(ty-fn @7.18-7.37 (effectful false)
					(ty @7.18-7.25 (name "Request"))
					(ty @7.29-7.37 (name "Response"))))))
	(d-let
		(p-assign @11.1-11.10 (ident "parseJson"))
		(e-lambda @11.13-11.38
			(args
				(p-assign @11.14-11.19 (ident "input")))
			(e-call @11.21-11.38
				(e-lookup-external @11.21-11.31
					(module-idx "1")
					(target-node-idx "0"))
				(e-lookup-local @11.32-11.37
					(p-assign @11.14-11.19 (ident "input")))))
		(annotation @11.1-11.10
			(declared-type
				(ty-fn @10.13-10.30 (effectful false)
					(ty @10.13-10.16 (name "Str"))
					(ty-lookup-external @10.20-10.30
						(ext-decl @10.20-10.30 (ident "Json.Value") (kind "type")))))))
	(d-let
		(p-assign @14.1-14.10 (ident "handleApi"))
		(e-lambda @14.13-20.2
			(args
				(p-assign @14.14-14.21 (ident "request")))
			(e-block @14.23-20.2
				(s-let @15.5-15.39
					(p-assign @15.5-15.11 (ident "result"))
					(e-call @15.14-15.39
						(e-lookup-external @15.14-15.25
							(module-idx "1")
							(target-node-idx "0"))
						(e-dot-access @15.26-15.38 (field "body")
							(receiver
								(e-lookup-local @15.26-15.33
									(p-assign @14.14-14.21 (ident "request")))))))
				(e-match @16.5-19.6
					(match @16.5-19.6
						(cond
							(e-lookup-local @16.11-16.17
								(p-assign @15.5-15.11 (ident "result"))))
						(branches
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag @17.9-17.17)))
								(value
									(e-tag @17.21-17.23 (name "Ok")
										(args
											(e-call @17.24-17.42
												(e-lookup-external @17.24-17.36
													(module-idx "0")
													(target-node-idx "0"))
												(e-lookup-local @17.37-17.41
													(p-assign @17.12-17.16 (ident "data"))))))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag @18.9-18.17)))
								(value
									(e-tag @18.21-18.24 (name "Err")
										(args
											(e-lookup-local @18.25-18.28
												(p-assign @18.13-18.16 (ident "err"))))))))))))
		(annotation @14.1-14.10
			(declared-type
				(ty-fn @13.13-13.62 (effectful false)
					(ty-lookup-external @13.13-13.25
						(ext-decl @13.13-13.25 (ident "Http.Request") (kind "type")))
					(ty-apply @13.29-13.62 (symbol "Result")
						(ty-lookup-external @13.36-13.49
							(ext-decl @13.36-13.49 (ident "Http.Response") (kind "type")))
						(ty-lookup-external @13.51-13.61
							(ext-decl @13.51-13.61 (ident "Json.Error") (kind "type"))))))))
	(d-let
		(p-assign @23.1-23.7 (ident "config"))
		(e-lookup-external @23.10-23.28
			(module-idx "1")
			(target-node-idx "0"))
		(annotation @23.1-23.7
			(declared-type
				(ty-lookup-external @22.10-22.21
					(ext-decl @22.10-22.21 (ident "Json.Config") (kind "type"))))))
	(d-let
		(p-assign @27.1-27.15 (ident "advancedParser"))
		(e-lambda @27.18-27.82
			(args
				(p-assign @27.19-27.31 (ident "parserConfig"))
				(p-assign @27.33-27.38 (ident "input")))
			(e-call @27.40-27.82
				(e-lookup-external @27.40-27.61
					(module-idx "1")
					(target-node-idx "0"))
				(e-lookup-local @27.62-27.74
					(p-assign @27.19-27.31 (ident "parserConfig")))
				(e-lookup-local @27.76-27.81
					(p-assign @27.33-27.38 (ident "input")))))
		(annotation @27.1-27.15
			(declared-type
				(ty-fn @26.18-26.82 (effectful false)
					(ty-lookup-external @26.18-26.36
						(ext-decl @26.18-26.36 (ident "Json.Parser.Config") (kind "type")))
					(ty @26.38-26.41 (name "Str"))
					(ty-apply @26.45-26.82 (symbol "Result")
						(ty-lookup-external @26.52-26.62
							(ext-decl @26.52-26.62 (ident "Json.Value") (kind "type")))
						(ty-lookup-external @26.64-26.81
							(ext-decl @26.64-26.81 (ident "Json.Parser.Error") (kind "type"))))))))
	(d-let
		(p-assign @31.1-31.15 (ident "combineResults"))
		(e-lambda @31.18-39.6
			(args
				(p-assign @31.19-31.26 (ident "result1"))
				(p-assign @31.28-31.35 (ident "result2")))
			(e-match @32.5-39.6
				(match @32.5-39.6
					(cond
						(e-lookup-local @32.11-32.18
							(p-assign @31.19-31.26 (ident "result1"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag @33.9-33.19)))
							(value
								(e-match @34.13-37.14
									(match @34.13-37.14
										(cond
											(e-lookup-local @34.19-34.26
												(p-assign @31.28-31.35 (ident "result2"))))
										(branches
											(branch
												(patterns
													(pattern (degenerate false)
														(p-applied-tag @35.17-35.27)))
												(value
													(e-tag @35.31-35.33 (name "Ok")
														(args
															(e-tuple @35.34-35.50
																(elems
																	(e-lookup-local @35.35-35.41
																		(p-assign @33.12-33.18 (ident "value1")))
																	(e-lookup-local @35.43-35.49
																		(p-assign @35.20-35.26 (ident "value2")))))))))
											(branch
												(patterns
													(pattern (degenerate false)
														(p-applied-tag @36.17-36.25)))
												(value
													(e-tag @36.29-36.32 (name "Err")
														(args
															(e-lookup-local @36.33-36.36
																(p-assign @36.21-36.24 (ident "err"))))))))))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag @38.9-38.17)))
							(value
								(e-tag @38.21-38.24 (name "Err")
									(args
										(e-lookup-local @38.25-38.28
											(p-assign @38.13-38.16 (ident "err")))))))))))
		(annotation @31.1-31.15
			(declared-type
				(ty-fn @30.18-30.71 (effectful false)
					(ty-apply @30.18-30.32 (symbol "Result")
						(ty-var @30.25-30.25 (name "a"))
						(ty-var @1.1-1.1 (name "err")))
					(ty-apply @30.34-30.48 (symbol "Result")
						(ty-var @30.41-30.41 (name "b"))
						(ty-var @1.1-1.1 (name "err")))
					(ty-apply @30.52-30.71 (symbol "Result")
						(ty-tuple @30.59-30.65
							(ty-var @30.60-30.60 (name "a"))
							(ty-var @1.1-1.1 (name "b")))
						(ty-var @1.1-1.1 (name "err")))))))
	(s-import @3.1-3.56 (module "http.Client") (qualifier "http") (alias "Http")
		(exposes
			(exposed (name "Request") (wildcard false))
			(exposed (name "Response") (wildcard false))))
	(s-import @4.1-4.17 (module "json.Json") (qualifier "json")
		(exposes))
	(s-import @5.1-5.38 (module "utils.Result") (qualifier "utils")
		(exposes
			(exposed (name "Result") (wildcard false))))
	(ext-decl @10.20-10.30 (ident "Json.Value") (kind "type"))
	(ext-decl @13.13-13.25 (ident "Http.Request") (kind "type"))
	(ext-decl @13.36-13.49 (ident "Http.Response") (kind "type"))
	(ext-decl @13.51-13.61 (ident "Json.Error") (kind "type"))
	(ext-decl @22.10-22.21 (ident "Json.Config") (kind "type"))
	(ext-decl @26.18-26.36 (ident "Json.Parser.Config") (kind "type"))
	(ext-decl @26.52-26.62 (ident "Json.Value") (kind "type"))
	(ext-decl @26.64-26.81 (ident "Json.Parser.Error") (kind "type")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @8.1-8.15 (type "Error -> Error"))
		(patt @11.1-11.10 (type "Str -> Json.Value"))
		(patt @14.1-14.10 (type "{ body: field } -> Error"))
		(patt @23.1-23.7 (type "Error"))
		(patt @27.1-27.15 (type "Json.Parser.Config, Str -> Error"))
		(patt @31.1-31.15 (type "Error, Error -> Error")))
	(expressions
		(expr @8.18-8.44 (type "Error -> Error"))
		(expr @11.13-11.38 (type "Str -> Json.Value"))
		(expr @14.13-20.2 (type "{ body: field } -> Error"))
		(expr @23.10-23.28 (type "Error"))
		(expr @27.18-27.82 (type "Json.Parser.Config, Str -> Error"))
		(expr @31.18-39.6 (type "Error, Error -> Error"))))
~~~

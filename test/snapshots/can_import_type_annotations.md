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
# TOKENS
~~~text
KwModule OpenSquare CloseSquare KwImport LowerIdent Dot UpperIdent KwAs UpperIdent KwExposing OpenSquare UpperIdent Comma UpperIdent CloseSquare KwImport LowerIdent Dot UpperIdent KwImport LowerIdent Dot UpperIdent KwExposing OpenSquare UpperIdent CloseSquare LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar UpperIdent Dot LowerIdent LowerIdent OpColon UpperIdent OpArrow UpperIdent Dot UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound LowerIdent OpColon UpperIdent Dot UpperIdent OpArrow UpperIdent OpenRound UpperIdent Dot UpperIdent Comma UpperIdent Dot UpperIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly LowerIdent OpAssign UpperIdent Dot LowerIdent OpenRound LowerIdent Dot LowerIdent CloseRound KwMatch LowerIdent OpenCurly UpperIdent OpenRound LowerIdent CloseRound OpFatArrow UpperIdent OpenRound UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound CloseRound UpperIdent OpenRound LowerIdent CloseRound OpFatArrow UpperIdent OpenRound LowerIdent CloseRound CloseCurly CloseCurly LowerIdent OpColon UpperIdent Dot UpperIdent LowerIdent OpAssign UpperIdent Dot LowerIdent LowerIdent OpColon UpperIdent Dot UpperIdent Dot UpperIdent Comma UpperIdent OpArrow UpperIdent OpenRound UpperIdent Dot UpperIdent Comma UpperIdent Dot UpperIdent Dot UpperIdent CloseRound LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar UpperIdent Dot UpperIdent Dot LowerIdent OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpColon UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound Comma UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpArrow UpperIdent OpenRound OpenRound LowerIdent Comma LowerIdent CloseRound Comma LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar KwMatch LowerIdent OpenCurly UpperIdent OpenRound LowerIdent CloseRound OpFatArrow KwMatch OpenRound LowerIdent CloseRound OpenCurly UpperIdent OpenRound LowerIdent CloseRound OpFatArrow UpperIdent OpenRound OpenRound LowerIdent Comma LowerIdent CloseRound CloseRound UpperIdent OpenRound LowerIdent CloseRound OpFatArrow UpperIdent OpenRound LowerIdent CloseRound CloseCurly UpperIdent OpenRound LowerIdent CloseRound OpFatArrow UpperIdent OpenRound LowerIdent CloseRound CloseCurly ~~~
# PARSE
~~~clojure
(module-header)
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
	match result
		Ok(data) => Ok(Http.success(data))
		Err(err) => Err(err)
}
config : Json.Config
config = Json.defaultConfig
advancedParser : Json.Parser | Config -> Str -> Result(Json.Value, Json.Parser | Error)
advancedParser = |parserConfig, input| Json.Parser | .parseWith((parserConfig, input))
combineResults : Result(a, err) -> Result(b, err) -> Result((a, b), err)
combineResults = |result1, result2| match result1
	Ok(value1) => 
		match result2
			Ok(value2) => Ok((value1, value2))
			Err(err) => Err(err)
	Err(err) => Err(err)
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 3:1 to 3:56

**Unsupported Node**
at 4:1 to 4:17

**Unsupported Node**
at 5:1 to 5:38

**Unsupported Node**
at 8:24 to 8:28

**Unsupported Node**
at 11:21 to 11:25

**Unsupported Node**
at 15:14 to 15:18

**Unsupported Node**
at 17:18 to 17:20

**Unsupported Node**
at 18:9 to 18:17

**Unsupported Node**
at 23:10 to 23:14

**Unsupported Node**
at 26:18 to 26:22

**Unsupported Node**
at 26:22 to 26:29

**Unsupported Node**
at 26:64 to 26:68

**Unsupported Node**
at 26:68 to 26:75

**Unsupported Node**
at 27:40 to 27:44

**Unsupported Node**
at 27:44 to 27:51

**Unsupported Node**
at 33:20 to 33:22

**Unsupported Node**
at 38:9 to 38:17

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "processRequest")
    (Expr.binop_thin_arrow
      (Expr.apply_tag)
      (Expr.apply_tag)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "processRequest")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.lookup "parseJson")
    (Expr.binop_thin_arrow
      (Expr.apply_tag)
      (Expr.module_access
        (Expr.malformed)
        (Expr.malformed)
      )
    )
  )
  (Expr.binop_equals
    (Expr.lookup "parseJson")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.lookup "handleApi")
    (Expr.binop_thin_arrow
      (Expr.module_access
        (Expr.malformed)
        (Expr.malformed)
      )
      (Expr.apply_tag)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "handleApi")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.lookup "config")
    (Expr.module_access
      (Expr.malformed)
      (Expr.malformed)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "config")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.lookup "advancedParser")
    (Expr.binop_thin_arrow
      (Expr.lambda)
      (Expr.binop_thin_arrow
        (Expr.apply_tag)
        (Expr.apply_tag)
      )
    )
  )
  (Expr.binop_equals
    (Expr.lookup "advancedParser")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.lookup "combineResults")
    (Expr.binop_thin_arrow
      (Expr.apply_tag)
      (Expr.binop_thin_arrow
        (Expr.apply_tag)
        (Expr.apply_tag)
      )
    )
  )
  (Expr.binop_equals
    (Expr.lookup "combineResults")
    (Expr.lambda)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_c")
~~~
# TYPES
~~~roc
processRequest : _c
parseJson : _c
handleApi : _c
config : _c
advancedParser : _c
combineResults : _c
~~~

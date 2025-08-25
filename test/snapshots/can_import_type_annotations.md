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
(block
  (import
    (lc "http")
    (uc "Client")
    (uc "Http")
    (uc "Request")
    (uc "Response")
  )
  (import
    (lc "json")
    (uc "Json")
  )
  (import
    (lc "utils")
    (uc "Result")
    (uc "Result")
  )
  (binop_colon
    (lc "processRequest")
    (binop_thin_arrow
      (uc "Request")
      (uc "Response")
    )
  )
  (binop_equals
    (lc "processRequest")
    (lambda
      (body
        (binop_pipe
          (uc "Http")
          (dot_lc "defaultResponse")
        )
      )
      (args
        (lc "req")
      )
    )
  )
  (binop_colon
    (lc "parseJson")
    (binop_thin_arrow
      (uc "Str")
      (binop_pipe
        (uc "Json")
        (uc "Value")
      )
    )
  )
  (binop_equals
    (lc "parseJson")
    (lambda
      (body
        (apply_anon
          (binop_pipe
            (uc "Json")
            (dot_lc "parse")
          )
          (lc "input")
        )
      )
      (args
        (lc "input")
      )
    )
  )
  (binop_colon
    (lc "handleApi")
    (binop_thin_arrow
      (binop_pipe
        (uc "Http")
        (uc "Request")
      )
      (apply_uc
        (uc "Result")
        (tuple_literal
          (binop_pipe
            (uc "Http")
            (uc "Response")
          )
          (binop_pipe
            (uc "Json")
            (uc "Error")
          )
        )
      )
    )
  )
  (binop_equals
    (lc "handleApi")
    (lambda
      (body
        (binop_thin_arrow
          (record_literal
            (binop_equals
              (lc "result")
              (apply_anon
                (binop_pipe
                  (uc "Json")
                  (dot_lc "decode")
                )
                (binop_pipe
                  (lc "request")
                  (dot_lc "body")
                )
              )
            )
            (match <74 branches>)
            (binop_colon
              (lc "config")
              (binop_pipe
                (uc "Json")
                (uc "Config")
              )
            )
            (binop_equals
              (lc "config")
              (binop_pipe
                (uc "Json")
                (dot_lc "defaultConfig")
              )
            )
            (binop_colon
              (lc "advancedParser")
              (binop_pipe
                (binop_pipe
                  (uc "Json")
                  (uc "Parser")
                )
                (uc "Config")
              )
            )
            (uc "Str")
          )
          (apply_uc
            (uc "Result")
            (tuple_literal
              (binop_pipe
                (uc "Json")
                (uc "Value")
              )
              (binop_pipe
                (binop_pipe
                  (uc "Json")
                  (uc "Parser")
                )
                (uc "Error")
              )
            )
          )
        )
      )
      (args
        (lc "request")
      )
    )
  )
  (binop_equals
    (lc "advancedParser")
    (lambda
      (body
        (apply_anon
          (binop_pipe
            (binop_pipe
              (uc "Json")
              (uc "Parser")
            )
            (dot_lc "parseWith")
          )
          (tuple_literal
            (lc "parserConfig")
            (lc "input")
          )
        )
      )
      (args
        (tuple_literal
          (lc "parserConfig")
          (lc "input")
        )
      )
    )
  )
  (binop_colon
    (lc "combineResults")
    (binop_thin_arrow
      (apply_uc
        (uc "Result")
        (tuple_literal
          (lc "a")
          (lc "err")
        )
      )
      (binop_thin_arrow
        (apply_uc
          (uc "Result")
          (tuple_literal
            (lc "b")
            (lc "err")
          )
        )
        (apply_uc
          (uc "Result")
          (tuple_literal
            (tuple_literal
              (lc "a")
              (lc "b")
            )
            (lc "err")
          )
        )
      )
    )
  )
  (binop_equals
    (lc "combineResults")
    (lambda
      (body
        (match <179 branches>)
      )
      (args
        (tuple_literal
          (lc "result1")
          (lc "result2")
        )
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
module []

import http.Client as Http exposing [Request, Response]
import json.Json
import utils.Result exposing [Result]
processRequest: (Request -> Response)
processRequest = \req -> Http.defaultResponse
parseJson: (Str -> Json.Value)
parseJson = \input -> Json.parse(input)

handleApi: (Http.Request -> Result((Http.Response, Json.Error)))
handleApi = \request -> {
	result = Json.decode(request.body),
	when result is {
		Ok(data)
		=>
		Ok(Http.success(data))
		Err(err)
		=>
		Err(err)
	} -> },
	config: Json.Config,
	config = Json.defaultConfig,
	advancedParser: Json.Parser | Config,
	Str
} -> Result((Json.Value, Json.Parser | Error))
advancedParser = \(
	parserConfig,
	input
) -> Json.Parser | .parseWith((parserConfig, input))
combineResults: (Result((a, err)) -> (Result((b, err)) -> Result(((a, b), err))))
combineResults = \(
	result1,
	result2
) -> when result1 is {
	Ok(value1)
	=>
	when result2 is {
		Ok(value2)
		=>
		Ok((value1, value2))
		Err(err)
		=>
		Err(err)
	} -> Err(err) => Err(err)
} -> 
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 16:5 to 16:18

**Parse Error**
at 17:18 to 17:18

**Parse Error**
at 18:18 to 18:18

**Parse Error**
at 16:5 to 20:1

**Parse Error**
at 20:1 to 20:1

**Parse Error**
at 14:23 to 26:42

**Parse Error**
at 32:5 to 32:19

**Parse Error**
at 33:20 to 33:20

**Parse Error**
at 34:13 to 34:28

**Parse Error**
at 35:28 to 35:28

**Parse Error**
at 36:26 to 36:26

**Parse Error**
at 34:13 to 38:9

**Parse Error**
at 32:5 to 39:6

**Parse Error**
at 39:6 to 39:6

**Unsupported Node**
at 3:1 to 3:55

**Unsupported Node**
at 4:1 to 4:17

**Unsupported Node**
at 5:1 to 5:37

**Unsupported Node**
at 7:18 to 7:37

**Unsupported Node**
at 8:18 to 8:24

**Unsupported Node**
at 10:13 to 10:29

**Unsupported Node**
at 11:13 to 11:21

**Unsupported Node**
at 13:13 to 13:62

**Unsupported Node**
at 14:13 to 14:23

**Unsupported Node**
at 27:18 to 27:40

**Unsupported Node**
at 30:18 to 31:1

**Unsupported Node**
at 31:18 to 32:5

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "processRequest")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "parseJson")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "handleApi")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "combineResults")
    (Expr.malformed)
  )
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
~~~

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
        (block
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
          (match
            (scrutinee               (lc "result")
))
        )
      )
      (args
        (lc "request")
      )
    )
  )
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
    (binop_thin_arrow
      (binop_pipe
        (binop_pipe
          (uc "Json")
          (uc "Parser")
        )
        (uc "Config")
      )
      (binop_thin_arrow
        (uc "Str")
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
        (match
          (scrutinee             (lc "result1")
))
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
processRequest : Request -> Response
processRequest = \req -> Http.defaultResponse
parseJson : Str -> Json.Value
parseJson = \input -> Json.parse(input)
handleApi : Http.Request -> Result (Http.Response, Json.Error)
handleApi = \request -> {
	result = Json.decode(request.body)
	match result
}
config : Json.Config
config = Json.defaultConfig
advancedParser : Json.Parser | Config -> Str -> Result (Json.Value, Json.Parser | Error)
advancedParser = \(parserConfig, input) -> Json.Parser | .parseWith((parserConfig, input))
combineResults : Result (a, err) -> Result (b, err) -> Result ((a, b), err)
combineResults = \(result1, result2) -> match result1
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 17:18 to 17:18

**Parse Error**
at 18:18 to 18:18

**Parse Error**
at 33:20 to 33:20

**Parse Error**
at 35:28 to 35:28

**Parse Error**
at 36:26 to 36:26

**Parse Error**
at 38:18 to 38:18

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_plus)
  (Expr.binop_plus)
  (Expr.binop_plus)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_c")
~~~
# TYPES
~~~roc
~~~

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
# TOKENS
~~~text
KwModule OpenSquare CloseSquare KwImport LowerIdent Dot UpperIdent KwExposing OpenSquare UpperIdent Comma UpperIdent Comma UpperIdent CloseSquare KwImport LowerIdent Dot UpperIdent KwAs UpperIdent KwExposing OpenSquare UpperIdent Comma UpperIdent Comma UpperIdent CloseSquare KwImport LowerIdent Dot UpperIdent KwExposing OpenSquare UpperIdent CloseSquare LowerIdent OpColon UpperIdent OpArrow UpperIdent OpenRound UpperIdent Comma UpperIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly LowerIdent OpAssign UpperIdent Dot LowerIdent OpenRound LowerIdent Dot LowerIdent CloseRound KwMatch LowerIdent OpenCurly UpperIdent OpenRound LowerIdent CloseRound OpFatArrow UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound UpperIdent OpenRound LowerIdent CloseRound OpFatArrow UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound CloseCurly CloseCurly LowerIdent OpColon UpperIdent Comma UpperIdent OpenRound UpperIdent CloseRound OpArrow UpperIdent OpenRound UpperIdent OpenRound UpperIdent CloseRound Comma UpperIdent CloseRound LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar UpperIdent Dot LowerIdent OpenRound LowerIdent Comma OpBar LowerIdent OpBar UpperIdent Dot LowerIdent OpenRound LowerIdent Comma LowerIdent CloseRound Comma CloseRound UpperIdent OpColon OpenCurly LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent Comma CloseCurly LowerIdent OpColon UpperIdent OpArrow UpperIdent Dot UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar KwMatch LowerIdent Dot LowerIdent OpenCurly UpperIdent OpenRound LowerIdent CloseRound OpFatArrow UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound UpperIdent OpenRound LowerIdent CloseRound OpFatArrow UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound CloseCurly LowerIdent OpColon UpperIdent OpenRound UpperIdent Comma UpperIdent CloseRound Comma UpperIdent OpArrow UpperIdent OpenRound UpperIdent Comma UpperIdent CloseRound LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar KwMatch LowerIdent OpenCurly UpperIdent OpenRound LowerIdent CloseRound OpFatArrow UpperIdent OpenRound OpenCurly LowerIdent OpColon UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound Comma LowerIdent OpColon LowerIdent CloseCurly CloseRound UpperIdent OpenRound LowerIdent CloseRound OpFatArrow UpperIdent OpenRound LowerIdent CloseRound CloseCurly ~~~
# PARSE
~~~clojure
(block
  (import
    (lc "json")
    (uc "Json")
    (uc "Value")
    (uc "Error")
    (uc "Config")
  )
  (import
    (lc "http")
    (uc "Client")
    (uc "Http")
    (uc "Request")
    (uc "Response")
    (uc "Status")
  )
  (import
    (lc "utils")
    (uc "Result")
    (uc "Result")
  )
  (binop_colon
    (lc "parseJson")
    (binop_thin_arrow
      (uc "Str")
      (apply_uc
        (uc "Result")
        (tuple_literal
          (uc "Value")
          (uc "Error")
        )
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
    (lc "handleRequest")
    (binop_thin_arrow
      (uc "Request")
      (uc "Response")
    )
  )
  (binop_equals
    (lc "handleRequest")
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
                  (lc "req")
                  (dot_lc "body")
                )
              )
            )
            (match <60 branches>)
            (binop_colon
              (lc "processData")
              (uc "Config")
            )
            (apply_uc
              (uc "List")
              (uc "Value")
            )
          )
          (apply_uc
            (uc "Result")
            (tuple_literal
              (apply_uc
                (uc "List")
                (uc "Value")
              )
              (uc "Error")
            )
          )
        )
      )
      (args
        (lc "req")
      )
    )
  )
  (binop_equals
    (lc "processData")
    (lambda
      (body
        (apply_anon
          (binop_pipe
            (uc "List")
            (dot_lc "mapTry")
          )
          (tuple_literal
            (lc "values")
            (lambda
              (body
                (tuple_literal
                  (apply_anon
                    (binop_pipe
                      (uc "Json")
                      (dot_lc "validateWith")
                    )
                    (tuple_literal
                      (lc "config")
                      (lc "v")
                    )
                  )
                  (malformed malformed:expr_unexpected_token)
                )
              )
              (args
                (lc "v")
              )
            )
          )
        )
      )
      (args
        (tuple_literal
          (lc "config")
          (lc "values")
        )
      )
    )
  )
  (binop_colon
    (uc "ServerConfig")
    (record_literal
      (binop_colon
        (lc "jsonConfig")
        (uc "Config")
      )
      (binop_colon
        (lc "httpStatus")
        (uc "Status")
      )
      (binop_colon
        (lc "defaultResponse")
        (uc "Response")
      )
    )
  )
  (binop_colon
    (lc "createClient")
    (binop_thin_arrow
      (uc "Config")
      (binop_pipe
        (uc "Http")
        (uc "Client")
      )
    )
  )
  (binop_equals
    (lc "createClient")
    (lambda
      (body
        (apply_anon
          (binop_pipe
            (uc "Http")
            (dot_lc "clientWith")
          )
          (lc "config")
        )
      )
      (args
        (lc "config")
      )
    )
  )
  (binop_colon
    (lc "handleResponse")
    (binop_thin_arrow
      (uc "Response")
      (uc "Str")
    )
  )
  (binop_equals
    (lc "handleResponse")
    (lambda
      (body
        (match <168 branches>)
      )
      (args
        (lc "response")
      )
    )
  )
  (binop_equals
    (lc "combineResults")
    (lambda
      (body
        (match <201 branches>)
      )
      (args
        (tuple_literal
          (lc "jsonResult")
          (lc "httpStatus")
        )
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
module []

import json.Json exposing [Value, Error, Config]
import http.Client as Http exposing [Request, Response, Status]
import utils.Result exposing [Result]
parseJson : Str -> Result (Value, Error)
parseJson = \input -> Json.parse(input)

# Test mixing exposed types with qualified access
handleRequest : Request -> Response
handleRequest = \req -> { result = Json.decode(req.body), match result {
        Ok(value) => Http.ok(value)
        Err(error) => Http.badRequest(error)
    }, 

# Test using exposed types in complex signatures
processData : Config, List(Value) } -> Result((List(Value), Error))
processData = \(config, values) -> List.mapTry((values, \v -> (Json.validateWith((config, v)))))ServerConfig :
	{
		jsonConfig : Config,
		httpStatus : Status,
		defaultResponse : Response,
	}
createClient : Config -> Http.Client
createClient = \config -> Http.clientWith(config)
handleResponse : Response -> Str
handleResponse = \response -> match response.status {
        Ok(status) => Http.statusToString(status)
        Err(error) => Error.toString(error)
    }combineResults = \(jsonResult, httpStatus) -> match jsonResult {
        Ok(value) => Ok({ body: Json.encode(value), status: httpStatus })
        Err(error) => Err(error)
    }
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 15:5 to 15:18

**Parse Error**
at 16:19 to 16:19

**Parse Error**
at 17:20 to 17:20

**Parse Error**
at 15:5 to 19:1

**Parse Error**
at 19:1 to 19:1

**Parse Error**
at 13:23 to 22:35

**Parse Error**
at 27:5 to 27:5

**Parse Error**
at 24:9 to 30:1

**Parse Error**
at 43:5 to 43:27

**Parse Error**
at 44:20 to 44:20

**Parse Error**
at 45:20 to 45:20

**Parse Error**
at 43:5 to 49:1

**Parse Error**
at 51:5 to 51:22

**Parse Error**
at 52:19 to 52:19

**Parse Error**
at 53:20 to 53:20

**Parse Error**
at 51:5 to 54:6

**Parse Error**
at 54:6 to 54:6

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
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~

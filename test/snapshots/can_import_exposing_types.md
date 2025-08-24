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
        (block
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
            (binop_thin_arrow
              (uc "Config")
              (binop_thin_arrow
                (apply_uc
                  (uc "List")
                  (uc "Value")
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
            (block
              (binop_colon
                (lc "jsonConfig")
                (tuple_literal
                  (binop_colon
                    (tuple_literal
                      (binop_colon
                        (tuple_literal
                          (uc "Config")
                          (lc "httpStatus")
                        )
                        (uc "Status")
                      )
                      (lc "defaultResponse")
                    )
                    (uc "Response")
                  )
                  (malformed malformed:expr_unexpected_token)
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
                    (match <163 branches>)
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
                    (match <196 branches>)
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
          )
        )
      )
      (args
        (lc "req")
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
UNDECLARED TYPE - can_import_exposing_types.md:31:18:31:24
UNDECLARED TYPE - can_import_exposing_types.md:32:18:32:24
UNDECLARED TYPE - can_import_exposing_types.md:33:23:33:31
MODULE NOT FOUND - can_import_exposing_types.md:3:1:3:49
MODULE NOT FOUND - can_import_exposing_types.md:4:1:4:64
MODULE NOT FOUND - can_import_exposing_types.md:5:1:5:38
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
at 27:5 to 27:5

**Parse Error**
at 24:9 to 30:1

**Parse Error**
at 34:1 to 34:1

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

**Parse Error**
at 30:16 to 54:6

**Parse Error**
at 13:23 to 54:6

**Unsupported Node**
at 3:1 to 3:48

**Unsupported Node**
at 4:1 to 4:63

**Unsupported Node**
at 5:1 to 5:37

**Unsupported Node**
at 8:13 to 9:1

**Unsupported Node**
at 9:13 to 9:21

**Unsupported Node**
at 12:17 to 12:36

**Unsupported Node**
at 13:17 to 13:23

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "parseJson")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "handleRequest")
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

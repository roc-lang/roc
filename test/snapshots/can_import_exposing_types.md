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
KwModule OpenSquare CloseSquare BlankLine KwImport LowerIdent Dot UpperIdent KwExposing OpenSquare UpperIdent Comma UpperIdent Comma UpperIdent CloseSquare KwImport LowerIdent Dot UpperIdent KwAs UpperIdent KwExposing OpenSquare UpperIdent Comma UpperIdent Comma UpperIdent CloseSquare KwImport LowerIdent Dot UpperIdent KwExposing OpenSquare UpperIdent CloseSquare BlankLine LineComment LowerIdent OpColon UpperIdent OpArrow UpperIdent OpenRound UpperIdent Comma UpperIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound BlankLine LineComment LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly LowerIdent OpAssign UpperIdent Dot LowerIdent OpenRound LowerIdent Dot LowerIdent CloseRound KwMatch LowerIdent OpenCurly UpperIdent OpenRound LowerIdent CloseRound OpThinArrow UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound UpperIdent OpenRound LowerIdent CloseRound OpThinArrow UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound CloseCurly CloseCurly BlankLine LineComment LowerIdent OpColon UpperIdent Comma UpperIdent OpenRound UpperIdent CloseRound OpArrow UpperIdent OpenRound UpperIdent OpenRound UpperIdent CloseRound Comma UpperIdent CloseRound LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar UpperIdent Dot LowerIdent OpenRound LowerIdent Comma OpBar LowerIdent OpBar UpperIdent Dot LowerIdent OpenRound LowerIdent Comma LowerIdent CloseRound Comma CloseRound BlankLine LineComment UpperIdent OpColon OpenCurly LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent Comma CloseCurly BlankLine LineComment LowerIdent OpColon UpperIdent OpArrow UpperIdent Dot UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound BlankLine LineComment LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar KwMatch LowerIdent Dot LowerIdent OpenCurly UpperIdent OpenRound LowerIdent CloseRound OpThinArrow UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound UpperIdent OpenRound LowerIdent CloseRound OpThinArrow UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound CloseCurly BlankLine LineComment LowerIdent OpColon UpperIdent OpenRound UpperIdent Comma UpperIdent CloseRound Comma UpperIdent OpArrow UpperIdent OpenRound UpperIdent Comma UpperIdent CloseRound LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar KwMatch LowerIdent OpenCurly UpperIdent OpenRound LowerIdent CloseRound OpThinArrow UpperIdent OpenRound OpenCurly LowerIdent OpColon UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound Comma LowerIdent OpColon LowerIdent CloseCurly CloseRound UpperIdent OpenRound LowerIdent CloseRound OpThinArrow UpperIdent OpenRound LowerIdent CloseRound CloseCurly ~~~
# PARSE
~~~clojure
(module-header)
(block
  (import
    (binop_exposing
      (binop_dot
        (lc "json")
        (uc "Json")
      )
      (list_literal
        (uc "Value")
        (uc "Error")
        (uc "Config")
      )
    )
  )
  (import
    (binop_exposing
      (binop_as
        (binop_dot
          (lc "http")
          (uc "Client")
        )
        (uc "Http")
      )
      (list_literal
        (uc "Request")
        (uc "Response")
        (uc "Status")
      )
    )
  )
  (import
    (binop_exposing
      (binop_dot
        (lc "utils")
        (uc "Result")
      )
      (list_literal
        (uc "Result")
      )
    )
  )
  (binop_colon
    (lc "parseJson")
    (binop_arrow_call
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
          (binop_dot
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
    (binop_arrow_call
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
              (binop_dot
                (uc "Json")
                (dot_lc "decode")
              )
              (binop_dot
                (lc "req")
                (dot_lc "body")
              )
            )
          )
          (apply_anon
            (match
              (scrutinee                 (lc "result")
)
              (branch1                 (binop_thick_arrow
                  (apply_uc
                    (uc "Ok")
                    (lc "value")
                  )
                  (malformed)
                )
))
            (lc "value")
          )
          (apply_uc
            (uc "Err")
            (lc "error")
          )
          (malformed)
          (apply_anon
            (binop_dot
              (uc "Http")
              (dot_lc "badRequest")
            )
            (lc "error")
          )
        )
      )
      (args
        (lc "req")
      )
    )
  )
  (malformed)
  (binop_colon
    (lc "processData")
    (binop_arrow_call
      (uc "Config")
      (binop_arrow_call
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
          (binop_dot
            (uc "List")
            (dot_lc "mapTry")
          )
          (tuple_literal
            (lc "values")
            (lambda
              (body
                (apply_anon
                  (binop_dot
                    (uc "Json")
                    (dot_lc "validateWith")
                  )
                  (tuple_literal
                    (lc "config")
                    (lc "v")
                  )
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
        (lc "config")
        (lc "values")
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
    (binop_arrow_call
      (uc "Config")
      (binop_dot
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
          (binop_dot
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
    (binop_arrow_call
      (uc "Response")
      (uc "Str")
    )
  )
  (binop_equals
    (lc "handleResponse")
    (lambda
      (body
        (apply_anon
          (match
            (scrutinee               (binop_dot
                (lc "response")
                (dot_lc "status")
              )
)
            (branch1               (binop_thick_arrow
                (apply_uc
                  (uc "Ok")
                  (lc "status")
                )
                (malformed)
              )
))
          (lc "status")
        )
      )
      (args
        (lc "response")
      )
    )
  )
  (apply_uc
    (uc "Err")
    (lc "error")
  )
  (malformed)
  (apply_anon
    (binop_dot
      (uc "Error")
      (dot_lc "toString")
    )
    (lc "error")
  )
  (malformed)
  (binop_colon
    (lc "combineResults")
    (binop_arrow_call
      (apply_uc
        (uc "Result")
        (tuple_literal
          (uc "Value")
          (uc "Error")
        )
      )
      (binop_arrow_call
        (uc "Status")
        (apply_uc
          (uc "Result")
          (tuple_literal
            (uc "Response")
            (uc "Error")
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
          (scrutinee             (lc "jsonResult")
)
          (branch1             (binop_thick_arrow
              (apply_uc
                (uc "Ok")
                (lc "value")
              )
              (malformed)
            )
))
      )
      (args
        (lc "jsonResult")
        (lc "httpStatus")
      )
    )
  )
  (lc "value")
  (malformed)
  (malformed)
  (binop_colon
    (lc "status")
    (lc "httpStatus")
  )
  (malformed)
  (malformed)
  (apply_uc
    (uc "Err")
    (lc "error")
  )
  (malformed)
  (apply_uc
    (uc "Err")
    (lc "error")
  )
  (malformed)
)
~~~
# FORMATTED
~~~roc
module []

import json.Json exposing [Value, Error, Config]
import (http.Client) as Http exposing [Request, Response, Status]
import utils.Result exposing [Result]
# Test using exposed types directly in annotations
parseJson : Str -> Result(Value, Error)
parseJson = |input| Json..parse(input)
# Test mixing exposed types with qualified access
handleRequest : Request -> Response
handleRequest = |req| {
	result = Json..decode(req..body)
	match result
		Ok(value) => Http
(value)
	Err(error)
	=>
	Http..badRequest(error)
}

}

# Test using exposed types in complex signatures
processData : Config -> List Value -> Result(List Value, Error)
processData = |config, values| List..mapTry((values, |v| Json..validateWith((config, v))))
# Test exposed types in record fields
ServerConfig :
	{
		jsonConfig: Config,
		httpStatus: Status,
		defaultResponse: Response,
	}
# Test exposed types with module-qualified usage
createClient : Config -> Http.Client
createClient = |config| Http..clientWith(config)
# Test nested type usage
handleResponse : Response -> Str
handleResponse = |response| match response..status
	Ok(status) => Http
(status)

Err(error)
=>
Error..toString(error)
}

# Test mixing exposed and qualified in same expression
combineResults : Result(Value, Error) -> Status -> Result(Response, Error)
combineResults = |jsonResult, httpStatus| match jsonResult
	Ok(value) => Ok

value
)
,
status : httpStatus
}
)
Err(error)
=>
Err(error)
}
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
**UNEXPECTED TOKEN IN EXPRESSION**
The token **Http** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**can_import_exposing_types.md:16:22:16:26:**
```roc
        Ok(value) => Http.ok(value)
```
                     ^^^^


**UNEXPECTED TOKEN IN PATTERN**
The token **.** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

**can_import_exposing_types.md:16:26:16:27:**
```roc
        Ok(value) => Http.ok(value)
```
                         ^


**PARSE ERROR**
A parsing error occurred: **expected_arrow_after_pattern**
This is an unexpected parsing error. Please check your syntax.

**can_import_exposing_types.md:16:27:16:29:**
```roc
        Ok(value) => Http.ok(value)
```
                          ^^


**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**can_import_exposing_types.md:16:26:16:29:**
```roc
        Ok(value) => Http.ok(value)
```
                         ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**can_import_exposing_types.md:17:20:17:23:**
```roc
        Err(error) => Http.badRequest(error)
```
                   ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}

# Test using exposed types in complex signatures
** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**can_import_exposing_types.md:19:1:22:1:**
```roc
}

# Test using exposed types in complex signatures
processData : Config, List(Value) -> Result(List(Value), Error)
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **Http** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**can_import_exposing_types.md:44:23:44:27:**
```roc
        Ok(status) => Http.statusToString(status)
```
                      ^^^^


**UNEXPECTED TOKEN IN PATTERN**
The token **.** is not expected in a pattern.
Patterns can contain identifiers, literals, lists, records, or tags.

**can_import_exposing_types.md:44:27:44:28:**
```roc
        Ok(status) => Http.statusToString(status)
```
                          ^


**PARSE ERROR**
A parsing error occurred: **expected_arrow_after_pattern**
This is an unexpected parsing error. Please check your syntax.

**can_import_exposing_types.md:44:28:44:42:**
```roc
        Ok(status) => Http.statusToString(status)
```
                           ^^^^^^^^^^^^^^


**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**can_import_exposing_types.md:44:27:44:42:**
```roc
        Ok(status) => Http.statusToString(status)
```
                          ^^^^^^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**can_import_exposing_types.md:45:20:45:23:**
```roc
        Err(error) => Error.toString(error)
```
                   ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}

# Test mixing exposed and qualified in same expression
** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**can_import_exposing_types.md:46:5:49:1:**
```roc
    }

# Test mixing exposed and qualified in same expression
combineResults : Result(Value, Error), Status -> Result(Response, Error)
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **Ok** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**can_import_exposing_types.md:52:22:52:24:**
```roc
        Ok(value) => Ok({ body: Json.encode(value), status: httpStatus })
```
                     ^^


**PARSE ERROR**
A parsing error occurred: **expected_expr_close_curly**
This is an unexpected parsing error. Please check your syntax.

**can_import_exposing_types.md:52:37:52:38:**
```roc
        Ok(value) => Ok({ body: Json.encode(value), status: httpStatus })
```
                                    ^


**PARSE ERROR**
A parsing error occurred: **expected_close_round**
This is an unexpected parsing error. Please check your syntax.

**can_import_exposing_types.md:52:38:52:44:**
```roc
        Ok(value) => Ok({ body: Json.encode(value), status: httpStatus })
```
                                     ^^^^^^


**PARSE ERROR**
A parsing error occurred: **expected_arrow_after_pattern**
This is an unexpected parsing error. Please check your syntax.

**can_import_exposing_types.md:52:44:52:45:**
```roc
        Ok(value) => Ok({ body: Json.encode(value), status: httpStatus })
```
                                           ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **)** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**can_import_exposing_types.md:52:50:52:51:**
```roc
        Ok(value) => Ok({ body: Json.encode(value), status: httpStatus })
```
                                                 ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **, ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**can_import_exposing_types.md:52:51:52:53:**
```roc
        Ok(value) => Ok({ body: Json.encode(value), status: httpStatus })
```
                                                  ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**can_import_exposing_types.md:52:72:52:73:**
```roc
        Ok(value) => Ok({ body: Json.encode(value), status: httpStatus })
```
                                                                       ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **)
        ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**can_import_exposing_types.md:52:73:53:9:**
```roc
        Ok(value) => Ok({ body: Json.encode(value), status: httpStatus })
        Err(error) => Err(error)
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**can_import_exposing_types.md:53:20:53:23:**
```roc
        Err(error) => Err(error)
```
                   ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**can_import_exposing_types.md:54:5:54:6:**
```roc
    }
```
    ^


**SHADOWING**
This definition shadows an existing one.

**can_import_exposing_types.md:8:1:8:10:**
```roc
parseJson : Str -> Result(Value, Error)
```
^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**can_import_exposing_types.md:9:1:9:10:**
```roc
parseJson = |input| Json.parse(input)
```
^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**can_import_exposing_types.md:12:1:12:14:**
```roc
handleRequest : Request -> Response
```
^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **error** in this scope.
Is there an **import** or **exposing** missing up-top?

**can_import_exposing_types.md:17:13:17:18:**
```roc
        Err(error) => Http.badRequest(error)
```
            ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **error** in this scope.
Is there an **import** or **exposing** missing up-top?

**can_import_exposing_types.md:17:39:17:44:**
```roc
        Err(error) => Http.badRequest(error)
```
                                      ^^^^^


**SHADOWING**
This definition shadows an existing one.

**can_import_exposing_types.md:13:1:13:14:**
```roc
handleRequest = |req| {
```
^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**can_import_exposing_types.md:22:1:22:12:**
```roc
processData : Config, List(Value) -> Result(List(Value), Error)
```
^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**can_import_exposing_types.md:23:1:23:12:**
```roc
processData = |config, values|
```
^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**can_import_exposing_types.md:37:1:37:13:**
```roc
createClient : Config -> Http.Client
```
^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**can_import_exposing_types.md:38:17:38:23:**
```roc
createClient = |config| Http.clientWith(config)
```
                ^^^^^^


**SHADOWING**
This definition shadows an existing one.

**can_import_exposing_types.md:38:1:38:13:**
```roc
createClient = |config| Http.clientWith(config)
```
^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**can_import_exposing_types.md:41:1:41:15:**
```roc
handleResponse : Response -> Str
```
^^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**can_import_exposing_types.md:44:12:44:18:**
```roc
        Ok(status) => Http.statusToString(status)
```
           ^^^^^^


**SHADOWING**
This definition shadows an existing one.

**can_import_exposing_types.md:42:1:42:15:**
```roc
handleResponse = |response|
```
^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **error** in this scope.
Is there an **import** or **exposing** missing up-top?

**can_import_exposing_types.md:45:13:45:18:**
```roc
        Err(error) => Error.toString(error)
```
            ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **error** in this scope.
Is there an **import** or **exposing** missing up-top?

**can_import_exposing_types.md:45:38:45:43:**
```roc
        Err(error) => Error.toString(error)
```
                                     ^^^^^


**SHADOWING**
This definition shadows an existing one.

**can_import_exposing_types.md:49:1:49:15:**
```roc
combineResults : Result(Value, Error), Status -> Result(Response, Error)
```
^^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**can_import_exposing_types.md:52:12:52:17:**
```roc
        Ok(value) => Ok({ body: Json.encode(value), status: httpStatus })
```
           ^^^^^


**SHADOWING**
This definition shadows an existing one.

**can_import_exposing_types.md:50:1:50:15:**
```roc
combineResults = |jsonResult, httpStatus|
```
^^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**can_import_exposing_types.md:52:53:52:59:**
```roc
        Ok(value) => Ok({ body: Json.encode(value), status: httpStatus })
```
                                                    ^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **error** in this scope.
Is there an **import** or **exposing** missing up-top?

**can_import_exposing_types.md:53:13:53:18:**
```roc
        Err(error) => Err(error)
```
            ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **error** in this scope.
Is there an **import** or **exposing** missing up-top?

**can_import_exposing_types.md:53:27:53:32:**
```roc
        Err(error) => Err(error)
```
                          ^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
  (Stmt.import)
  (Stmt.import)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "parseJson"))
    (type type_35)
  )
  (Stmt.assign
    (pattern (Patt.ident "parseJson"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "handleRequest"))
    (type type_49)
  )
  (Stmt.assign
    (pattern (Patt.ident "handleRequest"))
    (Expr.lambda (canonicalized))
  )
  (Expr.malformed)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "processData"))
    (type type_99)
  )
  (Stmt.assign
    (pattern (Patt.ident "processData"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_alias)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "createClient"))
    (type type_138)
  )
  (Stmt.assign
    (pattern (Patt.ident "createClient"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "handleResponse"))
    (type type_152)
  )
  (Stmt.assign
    (pattern (Patt.ident "handleResponse"))
    (Expr.lambda (canonicalized))
  )
  (Expr.tag_applied)
  (Expr.malformed)
  (Expr.fn_call)
  (Expr.malformed)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "combineResults"))
    (type type_194)
  )
  (Stmt.assign
    (pattern (Patt.ident "combineResults"))
    (Expr.lambda (canonicalized))
  )
  (Expr.lookup "value")
  (Expr.malformed)
  (Expr.malformed)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "status"))
    (type type_219)
  )
  (Expr.malformed)
  (Expr.malformed)
  (Expr.tag_applied)
  (Expr.malformed)
  (Expr.tag_applied)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 275
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 _)
(var #11 _)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 _)
(var #16 _)
(var #17 _)
(var #18 _)
(var #19 _)
(var #20 _)
(var #21 _)
(var #22 _)
(var #23 _)
(var #24 _)
(var #25 _)
(var #26 _)
(var #27 _)
(var #28 _)
(var #29 _)
(var #30 _)
(var #31 _)
(var #32 _)
(var #33 _)
(var #34 _)
(var #35 _)
(var #36 _)
(var #37 -> #234)
(var #38 _)
(var #39 _)
(var #40 _)
(var #41 -> #233)
(var #42 _)
(var #43 _)
(var #44 -> #234)
(var #45 _)
(var #46 _)
(var #47 _)
(var #48 _)
(var #49 _)
(var #50 _)
(var #51 -> #241)
(var #52 _)
(var #53 -> #60)
(var #54 _)
(var #55 _)
(var #56 -> #236)
(var #57 _)
(var #58 _)
(var #59 _)
(var #60 _)
(var #61 _)
(var #62 _)
(var #63 _)
(var #64 _)
(var #65 _)
(var #66 _)
(var #67 _)
(var #68 _)
(var #69 _)
(var #70 -> #237)
(var #71 _)
(var #72 _)
(var #73 -> #238)
(var #74 _)
(var #75 _)
(var #76 _)
(var #77 _)
(var #78 _)
(var #79 -> #240)
(var #80 _)
(var #81 _)
(var #82 _)
(var #83 -> #241)
(var #84 _)
(var #85 _)
(var #86 _)
(var #87 _)
(var #88 _)
(var #89 _)
(var #90 _)
(var #91 _)
(var #92 _)
(var #93 _)
(var #94 _)
(var #95 _)
(var #96 _)
(var #97 _)
(var #98 _)
(var #99 _)
(var #100 _)
(var #101 -> #252)
(var #102 _)
(var #103 _)
(var #104 _)
(var #105 _)
(var #106 -> #250)
(var #107 _)
(var #108 _)
(var #109 _)
(var #110 _)
(var #111 -> #247)
(var #112 _)
(var #113 _)
(var #114 -> #246)
(var #115 _)
(var #116 -> #248)
(var #117 -> #249)
(var #118 _)
(var #119 -> #252)
(var #120 _)
(var #121 _)
(var #122 _)
(var #123 _)
(var #124 _)
(var #125 _)
(var #126 _)
(var #127 _)
(var #128 _)
(var #129 _)
(var #130 _)
(var #131 _)
(var #132 _)
(var #133 _)
(var #134 _)
(var #135 _)
(var #136 _)
(var #137 _)
(var #138 _)
(var #139 _)
(var #140 -> #255)
(var #141 _)
(var #142 _)
(var #143 _)
(var #144 -> #254)
(var #145 _)
(var #146 _)
(var #147 -> #255)
(var #148 _)
(var #149 _)
(var #150 _)
(var #151 _)
(var #152 _)
(var #153 _)
(var #154 -> #258)
(var #155 _)
(var #156 _)
(var #157 _)
(var #158 _)
(var #159 _)
(var #160 _)
(var #161 _)
(var #162 _)
(var #163 _)
(var #164 _)
(var #165 _)
(var #166 -> #257)
(var #167 _)
(var #168 _)
(var #169 -> #258)
(var #170 _)
(var #171 -> #259)
(var #172 _)
(var #173 _)
(var #174 _)
(var #175 _)
(var #176 _)
(var #177 -> #261)
(var #178 _)
(var #179 _)
(var #180 _)
(var #181 _)
(var #182 _)
(var #183 _)
(var #184 _)
(var #185 _)
(var #186 _)
(var #187 _)
(var #188 _)
(var #189 _)
(var #190 _)
(var #191 _)
(var #192 _)
(var #193 _)
(var #194 _)
(var #195 _)
(var #196 -> #266)
(var #197 _)
(var #198 _)
(var #199 _)
(var #200 _)
(var #201 _)
(var #202 _)
(var #203 _)
(var #204 _)
(var #205 _)
(var #206 _)
(var #207 _)
(var #208 _)
(var #209 _)
(var #210 _)
(var #211 _)
(var #212 _)
(var #213 -> #266)
(var #214 _)
(var #215 _)
(var #216 _)
(var #217 _)
(var #218 _)
(var #219 _)
(var #220 _)
(var #221 _)
(var #222 _)
(var #223 -> #271)
(var #224 _)
(var #225 _)
(var #226 _)
(var #227 -> #273)
(var #228 _)
(var #229 _)
(var #230 _)
(var #231 _)
(var #232 _)
(var #233 fn_pure)
(var #234 fn_pure)
(var #235 _)
(var #236 fn_pure)
(var #237 fn_pure)
(var #238 fn_pure)
(var #239 _)
(var #240 fn_pure)
(var #241 fn_pure)
(var #242 _)
(var #243 _)
(var #244 _)
(var #245 _)
(var #246 tuple)
(var #247 fn_pure)
(var #248 fn_pure)
(var #249 tuple)
(var #250 fn_pure)
(var #251 fn_pure)
(var #252 fn_pure)
(var #253 _)
(var #254 fn_pure)
(var #255 fn_pure)
(var #256 _)
(var #257 fn_pure)
(var #258 fn_pure)
(var #259 fn_pure)
(var #260 _)
(var #261 fn_pure)
(var #262 _)
(var #263 _)
(var #264 _)
(var #265 fn_pure)
(var #266 fn_pure)
(var #267 _)
(var #268 _)
(var #269 _)
(var #270 _)
(var #271 fn_pure)
(var #272 _)
(var #273 fn_pure)
(var #274 _)
~~~
# TYPES
~~~roc
response : _a
processData : _arg -> _arg2 -> _ret
httpStatus : _a
handleRequest : _arg -> _ret
config : _a
value : _a
input : _a
createClient : _arg -> _ret
combineResults : _arg -> _arg2 -> _ret
values : _a
status : _a
jsonResult : _a
req : _a
parseJson : _arg -> _ret
handleResponse : _arg -> _ret
v : _a
~~~

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
KwModule OpenSquare CloseSquare BlankLine KwImport LowerIdent Dot UpperIdent KwExposing OpenSquare UpperIdent Comma UpperIdent Comma UpperIdent CloseSquare KwImport LowerIdent Dot UpperIdent KwAs UpperIdent KwExposing OpenSquare UpperIdent Comma UpperIdent Comma UpperIdent CloseSquare KwImport LowerIdent Dot UpperIdent KwExposing OpenSquare UpperIdent CloseSquare BlankLine LineComment LowerIdent OpColon UpperIdent OpArrow UpperIdent OpenRound UpperIdent Comma UpperIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound BlankLine LineComment LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly LowerIdent OpAssign UpperIdent Dot LowerIdent OpenRound LowerIdent Dot LowerIdent CloseRound KwMatch LowerIdent OpenCurly UpperIdent OpenRound LowerIdent CloseRound OpFatArrow UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound UpperIdent OpenRound LowerIdent CloseRound OpFatArrow UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound CloseCurly CloseCurly BlankLine LineComment LowerIdent OpColon UpperIdent Comma UpperIdent OpenRound UpperIdent CloseRound OpArrow UpperIdent OpenRound UpperIdent OpenRound UpperIdent CloseRound Comma UpperIdent CloseRound LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar UpperIdent Dot LowerIdent OpenRound LowerIdent Comma OpBar LowerIdent OpBar UpperIdent Dot LowerIdent OpenRound LowerIdent Comma LowerIdent CloseRound Comma CloseRound BlankLine LineComment UpperIdent OpColon OpenCurly LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent Comma LowerIdent OpColon UpperIdent Comma CloseCurly BlankLine LineComment LowerIdent OpColon UpperIdent OpArrow UpperIdent Dot UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound BlankLine LineComment LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar KwMatch LowerIdent Dot LowerIdent OpenCurly UpperIdent OpenRound LowerIdent CloseRound OpFatArrow UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound UpperIdent OpenRound LowerIdent CloseRound OpFatArrow UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound CloseCurly BlankLine LineComment LowerIdent OpColon UpperIdent OpenRound UpperIdent Comma UpperIdent CloseRound Comma UpperIdent OpArrow UpperIdent OpenRound UpperIdent Comma UpperIdent CloseRound LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar KwMatch LowerIdent OpenCurly UpperIdent OpenRound LowerIdent CloseRound OpFatArrow UpperIdent OpenRound OpenCurly LowerIdent OpColon UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound Comma LowerIdent OpColon LowerIdent CloseCurly CloseRound UpperIdent OpenRound LowerIdent CloseRound OpFatArrow UpperIdent OpenRound LowerIdent CloseRound CloseCurly ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
~~~roc
module []

import json.Json exposing [Value, Error, Config]
import http.Client as Http exposing [Request, Response, Status]
import utils.Result exposing [Result]
parseJson : Str -> Result(Value, Error)
parseJson = |input| Json.parse(input)
handleRequest : Request -> Response
handleRequest = |req| {
	result = Json.decode(req.body)
	match result
		Ok(value) => Http
(value)
	Err(error)
	=> 
	Http.badRequest(error)
}

}

# Test using exposed types in complex signatures
processData : Config -> List Value -> Result(List Value, Error)
processData = |config, values| List.mapTry((values, |v| Json.validateWith((config, v))))
ServerConfig :
	{
		jsonConfig : Config,
		httpStatus : Status,
		defaultResponse : Response,
	}
createClient : Config -> Http.Client
createClient = |config| Http.clientWith(config)
handleResponse : Response -> Str
handleResponse = |response| match response.status
	Ok(status) => Http
(status)

Err(error)
=> 
Error.toString(error)
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
NIL
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


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_exposing_types.md:3:1:3:49:**
```roc
import json.Json exposing [Value, Error, Config]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_exposing_types.md:4:1:4:64:**
```roc
import http.Client as Http exposing [Request, Response, Status]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_exposing_types.md:5:1:5:38:**
```roc
import utils.Result exposing [Result]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_exposing_types.md:9:21:9:25:**
```roc
parseJson = |input| Json.parse(input)
```
                    ^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_exposing_types.md:14:14:14:18:**
```roc
    result = Json.decode(req.body)
```
             ^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_exposing_types.md:16:9:16:26:**
```roc
        Ok(value) => Http.ok(value)
```
        ^^^^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_exposing_types.md:17:23:17:27:**
```roc
        Err(error) => Http.badRequest(error)
```
                      ^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_exposing_types.md:24:5:24:9:**
```roc
    List.mapTry(
```
    ^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_exposing_types.md:26:13:26:17:**
```roc
        |v| Json.validateWith(config, v),
```
            ^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_exposing_types.md:38:25:38:29:**
```roc
createClient = |config| Http.clientWith(config)
```
                        ^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_exposing_types.md:44:9:44:27:**
```roc
        Ok(status) => Http.statusToString(status)
```
        ^^^^^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_exposing_types.md:45:23:45:28:**
```roc
        Err(error) => Error.toString(error)
```
                      ^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_exposing_types.md:52:9:52:24:**
```roc
        Ok(value) => Ok({ body: Json.encode(value), status: httpStatus })
```
        ^^^^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "parseJson")
    (Expr.binop_thin_arrow
      (Expr.apply_tag)
      (Expr.apply_tag)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "parseJson")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.lookup "handleRequest")
    (Expr.binop_thin_arrow
      (Expr.apply_tag)
      (Expr.apply_tag)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "handleRequest")
    (Expr.lambda)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "processData")
    (Expr.binop_thin_arrow
      (Expr.apply_tag)
      (Expr.binop_thin_arrow
        (Expr.apply_tag)
        (Expr.apply_tag)
      )
    )
  )
  (Expr.binop_equals
    (Expr.lookup "processData")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.record_literal
      (Expr.binop_colon
        (Expr.lookup "jsonConfig")
        (Expr.apply_tag)
      )
      (Expr.binop_colon
        (Expr.lookup "httpStatus")
        (Expr.apply_tag)
      )
      (Expr.binop_colon
        (Expr.lookup "defaultResponse")
        (Expr.apply_tag)
      )
    )
  )
  (Expr.binop_colon
    (Expr.lookup "createClient")
    (Expr.binop_thin_arrow
      (Expr.apply_tag)
      (Expr.module_access
        (Expr.malformed)
        (Expr.malformed)
      )
    )
  )
  (Expr.binop_equals
    (Expr.lookup "createClient")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.lookup "handleResponse")
    (Expr.binop_thin_arrow
      (Expr.apply_tag)
      (Expr.apply_tag)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "handleResponse")
    (Expr.lambda)
  )
  (Expr.apply_tag)
  (Expr.malformed)
  (Expr.apply_ident)
  (Expr.malformed)
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
  (Expr.lookup "value")
  (Expr.malformed)
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "status")
    (Expr.lookup "httpStatus")
  )
  (Expr.malformed)
  (Expr.malformed)
  (Expr.apply_tag)
  (Expr.malformed)
  (Expr.apply_tag)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
parseJson : _a
handleRequest : _a
processData : _a
createClient : _a
handleResponse : _a
combineResults : _a
~~~

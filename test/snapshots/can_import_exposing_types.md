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

# Test using exposed types directly in annotations
parseJson : Str -> Result(Value, Error)
parseJson = |input| Json.parse(input)

# Test mixing exposed types with qualified access
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

# Test exposed types in record fields
ServerConfig :
	{
		jsonConfig : Config,
		httpStatus : Status,
		defaultResponse : Response,
	}

# Test exposed types with module-qualified usage
createClient : Config -> Http.Client
createClient = |config| Http.clientWith(config)

# Test nested type usage
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


**UNDEFINED VARIABLE**
Nothing is named **Json.parse** in this scope.
Is there an **import** or **exposing** missing up-top?

**can_import_exposing_types.md:9:21:9:31:**
```roc
parseJson = |input| Json.parse(input)
```
                    ^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **Json.decode** in this scope.
Is there an **import** or **exposing** missing up-top?

**can_import_exposing_types.md:14:14:14:25:**
```roc
    result = Json.decode(req.body)
```
             ^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_exposing_types.md:14:26:14:34:**
```roc
    result = Json.decode(req.body)
```
                         ^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_exposing_types.md:16:9:16:26:**
```roc
        Ok(value) => Http.ok(value)
```
        ^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **value** in this scope.
Is there an **import** or **exposing** missing up-top?

**can_import_exposing_types.md:16:30:16:35:**
```roc
        Ok(value) => Http.ok(value)
```
                             ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **error** in this scope.
Is there an **import** or **exposing** missing up-top?

**can_import_exposing_types.md:17:13:17:18:**
```roc
        Err(error) => Http.badRequest(error)
```
            ^^^^^


**UNDEFINED VARIABLE**
Nothing is named **Http.badRequest** in this scope.
Is there an **import** or **exposing** missing up-top?

**can_import_exposing_types.md:17:23:17:38:**
```roc
        Err(error) => Http.badRequest(error)
```
                      ^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **error** in this scope.
Is there an **import** or **exposing** missing up-top?

**can_import_exposing_types.md:17:39:17:44:**
```roc
        Err(error) => Http.badRequest(error)
```
                                      ^^^^^


**UNUSED VARIABLE**
Variable **req** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_req` to suppress this warning.
The unused variable is declared here:

**can_import_exposing_types.md:13:18:13:21:**
```roc
handleRequest = |req| {
```
                 ^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_exposing_types.md:19:1:22:1:**
```roc
}

# Test using exposed types in complex signatures
processData : Config, List(Value) -> Result(List(Value), Error)
```


**UNDEFINED VARIABLE**
Nothing is named **Json.validateWith** in this scope.
Is there an **import** or **exposing** missing up-top?

**can_import_exposing_types.md:26:13:26:30:**
```roc
        |v| Json.validateWith(config, v),
```
            ^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **Http.clientWith** in this scope.
Is there an **import** or **exposing** missing up-top?

**can_import_exposing_types.md:38:25:38:40:**
```roc
createClient = |config| Http.clientWith(config)
```
                        ^^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_exposing_types.md:43:11:43:26:**
```roc
    match response.status {
```
          ^^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_exposing_types.md:44:9:44:27:**
```roc
        Ok(status) => Http.statusToString(status)
```
        ^^^^^^^^^^^^^^^^^^


**UNUSED VARIABLE**
Variable **response** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_response` to suppress this warning.
The unused variable is declared here:

**can_import_exposing_types.md:42:19:42:27:**
```roc
handleResponse = |response|
```
                  ^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_exposing_types.md:45:9:45:19:**
```roc
        Err(error) => Error.toString(error)
```
        ^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_exposing_types.md:45:20:45:23:**
```roc
        Err(error) => Error.toString(error)
```
                   ^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_exposing_types.md:45:23:45:44:**
```roc
        Err(error) => Error.toString(error)
```
                      ^^^^^^^^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_exposing_types.md:46:5:49:1:**
```roc
    }

# Test mixing exposed and qualified in same expression
combineResults : Result(Value, Error), Status -> Result(Response, Error)
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_exposing_types.md:52:9:52:24:**
```roc
        Ok(value) => Ok({ body: Json.encode(value), status: httpStatus })
```
        ^^^^^^^^^^^^^^^


**UNUSED VARIABLE**
Variable **httpStatus** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_httpStatus` to suppress this warning.
The unused variable is declared here:

**can_import_exposing_types.md:50:31:50:41:**
```roc
combineResults = |jsonResult, httpStatus|
```
                              ^^^^^^^^^^


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**can_import_exposing_types.md:52:45:52:50:**
```roc
        Ok(value) => Ok({ body: Json.encode(value), status: httpStatus })
```
                                            ^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_exposing_types.md:52:50:52:51:**
```roc
        Ok(value) => Ok({ body: Json.encode(value), status: httpStatus })
```
                                                 ^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_exposing_types.md:52:51:52:53:**
```roc
        Ok(value) => Ok({ body: Json.encode(value), status: httpStatus })
```
                                                  ^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_exposing_types.md:52:72:52:73:**
```roc
        Ok(value) => Ok({ body: Json.encode(value), status: httpStatus })
```
                                                                       ^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_exposing_types.md:52:73:53:9:**
```roc
        Ok(value) => Ok({ body: Json.encode(value), status: httpStatus })
        Err(error) => Err(error)
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_exposing_types.md:53:9:53:19:**
```roc
        Err(error) => Err(error)
```
        ^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_exposing_types.md:53:20:53:23:**
```roc
        Err(error) => Err(error)
```
                   ^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_exposing_types.md:53:23:53:33:**
```roc
        Err(error) => Err(error)
```
                      ^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_exposing_types.md:54:5:54:6:**
```roc
    }
```
    ^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
  (Stmt.import)
  (Stmt.import)
  (Stmt.type_anno
    (name "parseJson")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "parseJson"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_anno
    (name "handleRequest")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "handleRequest"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.malformed)
  (Stmt.type_anno
    (name "processData")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "processData"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_anno
    (name node:uc)
    (type record_literal)
  )
  (Stmt.type_anno
    (name "createClient")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "createClient"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_anno
    (name "handleResponse")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "handleResponse"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.type_anno
    (name "combineResults")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "combineResults"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.type_anno
    (name "status")
    (type lc)
  )
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
  (Stmt.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~

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
KwModule OpenSquare CloseSquare BlankLine KwImport LowerIdent Dot UpperIdent KwAs UpperIdent KwExposing OpenSquare UpperIdent Comma UpperIdent CloseSquare KwImport LowerIdent Dot UpperIdent KwImport LowerIdent Dot UpperIdent KwExposing OpenSquare UpperIdent CloseSquare BlankLine LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar UpperIdent Dot LowerIdent BlankLine LowerIdent OpColon UpperIdent OpArrow UpperIdent Dot UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound BlankLine LowerIdent OpColon UpperIdent Dot UpperIdent OpArrow UpperIdent OpenRound UpperIdent Dot UpperIdent Comma UpperIdent Dot UpperIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly LowerIdent OpAssign UpperIdent Dot LowerIdent OpenRound LowerIdent Dot LowerIdent CloseRound KwMatch LowerIdent OpenCurly UpperIdent OpenRound LowerIdent CloseRound OpFatArrow UpperIdent OpenRound UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound CloseRound UpperIdent OpenRound LowerIdent CloseRound OpFatArrow UpperIdent OpenRound LowerIdent CloseRound CloseCurly CloseCurly BlankLine LowerIdent OpColon UpperIdent Dot UpperIdent LowerIdent OpAssign UpperIdent Dot LowerIdent BlankLine LineComment LowerIdent OpColon UpperIdent Dot UpperIdent Dot UpperIdent Comma UpperIdent OpArrow UpperIdent OpenRound UpperIdent Dot UpperIdent Comma UpperIdent Dot UpperIdent Dot UpperIdent CloseRound LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar UpperIdent Dot UpperIdent Dot LowerIdent OpenRound LowerIdent Comma LowerIdent CloseRound BlankLine LineComment LowerIdent OpColon UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound Comma UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpArrow UpperIdent OpenRound OpenRound LowerIdent Comma LowerIdent CloseRound Comma LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar KwMatch LowerIdent OpenCurly UpperIdent OpenRound LowerIdent CloseRound OpFatArrow KwMatch OpenRound LowerIdent CloseRound OpenCurly UpperIdent OpenRound LowerIdent CloseRound OpFatArrow UpperIdent OpenRound OpenRound LowerIdent Comma LowerIdent CloseRound CloseRound UpperIdent OpenRound LowerIdent CloseRound OpFatArrow UpperIdent OpenRound LowerIdent CloseRound CloseCurly UpperIdent OpenRound LowerIdent CloseRound OpFatArrow UpperIdent OpenRound LowerIdent CloseRound CloseCurly ~~~
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
		Ok(data) => Ok
(data)
	)
	Err(err)
	=> 
	Err(err)
}

}

config : Json.Config
config = Json.defaultConfig

# Test nested type qualification
advancedParser : Json.Parser | Config -> Str -> Result(Json.Value, Json.Parser | Error)
advancedParser = |parserConfig, input| Json.Parser | .parseWith((parserConfig, input))

# Test function with multiple type parameters
combineResults : Result(a, err) -> Result(b, err) -> Result((a, b), err)
combineResults = |result1, result2| match result1
	Ok(value1) => match result2
		Ok(value2) => Ok
(err) => Err(err)

Err(err)
=> 
Err(err)
}
~~~
# EXPECTED
MODULE NOT FOUND - can_import_type_annotations.md:3:1:3:56
MODULE NOT FOUND - can_import_type_annotations.md:4:1:4:17
MODULE NOT FOUND - can_import_type_annotations.md:5:1:5:38
UNDECLARED TYPE - can_import_type_annotations.md:7:18:7:25
UNDECLARED TYPE - can_import_type_annotations.md:7:29:7:37
UNUSED VARIABLE - can_import_type_annotations.md:8:19:8:22
MODULE NOT IMPORTED - can_import_type_annotations.md:26:18:26:36
MODULE NOT IMPORTED - can_import_type_annotations.md:26:64:26:81
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **Ok** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**can_import_type_annotations.md:17:21:17:23:**
```roc
        Ok(data) => Ok(Http.success(data))
```
                    ^^


**PARSE ERROR**
A parsing error occurred: **expected_close_round**
This is an unexpected parsing error. Please check your syntax.

**can_import_type_annotations.md:17:28:17:29:**
```roc
        Ok(data) => Ok(Http.success(data))
```
                           ^


**PARSE ERROR**
A parsing error occurred: **expected_arrow_after_pattern**
This is an unexpected parsing error. Please check your syntax.

**can_import_type_annotations.md:17:29:17:36:**
```roc
        Ok(data) => Ok(Http.success(data))
```
                            ^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **)
        ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**can_import_type_annotations.md:17:42:18:9:**
```roc
        Ok(data) => Ok(Http.success(data))
        Err(err) => Err(err)
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**can_import_type_annotations.md:18:18:18:21:**
```roc
        Err(err) => Err(err)
```
                 ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}

** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**can_import_type_annotations.md:20:1:22:1:**
```roc
}

config : Json.Config
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **Ok** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**can_import_type_annotations.md:35:31:35:33:**
```roc
                Ok(value2) => Ok((value1, value2))
```
                              ^^


**PARSE ERROR**
A parsing error occurred: **expected_arrow_after_pattern**
This is an unexpected parsing error. Please check your syntax.

**can_import_type_annotations.md:36:17:36:20:**
```roc
                Err(err) => Err(err)
```
                ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **=> ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**can_import_type_annotations.md:38:18:38:21:**
```roc
        Err(err) => Err(err)
```
                 ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**can_import_type_annotations.md:39:5:39:6:**
```roc
    }
```
    ^


**UNDEFINED VARIABLE**
Nothing is named **Http.defaultResponse** in this scope.
Is there an **import** or **exposing** missing up-top?

**can_import_type_annotations.md:8:24:8:44:**
```roc
processRequest = |req| Http.defaultResponse
```
                       ^^^^^^^^^^^^^^^^^^^^


**UNUSED VARIABLE**
Variable **req** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_req` to suppress this warning.
The unused variable is declared here:

**can_import_type_annotations.md:8:19:8:22:**
```roc
processRequest = |req| Http.defaultResponse
```
                  ^^^


**UNDEFINED VARIABLE**
Nothing is named **Json.parse** in this scope.
Is there an **import** or **exposing** missing up-top?

**can_import_type_annotations.md:11:21:11:31:**
```roc
parseJson = |input| Json.parse(input)
```
                    ^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **Json.decode** in this scope.
Is there an **import** or **exposing** missing up-top?

**can_import_type_annotations.md:15:14:15:25:**
```roc
    result = Json.decode(request.body)
```
             ^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_type_annotations.md:15:26:15:38:**
```roc
    result = Json.decode(request.body)
```
                         ^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_type_annotations.md:17:9:17:23:**
```roc
        Ok(data) => Ok(Http.success(data))
```
        ^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **data** in this scope.
Is there an **import** or **exposing** missing up-top?

**can_import_type_annotations.md:17:37:17:41:**
```roc
        Ok(data) => Ok(Http.success(data))
```
                                    ^^^^


**UNDEFINED VARIABLE**
Nothing is named **err** in this scope.
Is there an **import** or **exposing** missing up-top?

**can_import_type_annotations.md:18:13:18:16:**
```roc
        Err(err) => Err(err)
```
            ^^^


**UNDEFINED VARIABLE**
Nothing is named **err** in this scope.
Is there an **import** or **exposing** missing up-top?

**can_import_type_annotations.md:18:25:18:28:**
```roc
        Err(err) => Err(err)
```
                        ^^^


**UNUSED VARIABLE**
Variable **request** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_request` to suppress this warning.
The unused variable is declared here:

**can_import_type_annotations.md:14:14:14:21:**
```roc
handleApi = |request| {
```
             ^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_type_annotations.md:20:1:22:1:**
```roc
}

config : Json.Config
```


**UNDEFINED VARIABLE**
Nothing is named **Json.defaultConfig** in this scope.
Is there an **import** or **exposing** missing up-top?

**can_import_type_annotations.md:23:10:23:28:**
```roc
config = Json.defaultConfig
```
         ^^^^^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_type_annotations.md:27:40:27:61:**
```roc
advancedParser = |parserConfig, input| Json.Parser.parseWith(parserConfig, input)
```
                                       ^^^^^^^^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_type_annotations.md:33:9:36:37:**
```roc
        Ok(value1) =>
            match(result2) {
                Ok(value2) => Ok((value1, value2))
                Err(err) => Err(err)
```


**UNUSED VARIABLE**
Variable **result2** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_result2` to suppress this warning.
The unused variable is declared here:

**can_import_type_annotations.md:31:28:31:35:**
```roc
combineResults = |result1, result2|
```
                           ^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_type_annotations.md:38:9:38:17:**
```roc
        Err(err) => Err(err)
```
        ^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_type_annotations.md:38:18:38:21:**
```roc
        Err(err) => Err(err)
```
                 ^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_type_annotations.md:38:21:38:29:**
```roc
        Err(err) => Err(err)
```
                    ^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_type_annotations.md:39:5:39:6:**
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
    (name "processRequest")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "processRequest"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_anno
    (name "parseJson")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "parseJson"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_anno
    (name "handleApi")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "handleApi"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.malformed)
  (Stmt.type_anno
    (name "config")
    (type binop_pipe)
  )
  (Stmt.assign
    (pattern (Patt.ident "config"))
    (Expr.module_access
      (Expr.malformed)
      (Expr.malformed)
    )
  )
  (Stmt.type_anno
    (name "advancedParser")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "advancedParser"))
    (Expr.lambda (canonicalized))
  )
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
  (Stmt.malformed)
)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
~~~

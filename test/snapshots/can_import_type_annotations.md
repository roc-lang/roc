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
advancedParser : Json.Parser | Config -> Str -> Result(Json.Value, Json.Parser | Error)
advancedParser = |parserConfig, input| Json.Parser | .parseWith((parserConfig, input))
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
NIL
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


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_type_annotations.md:3:1:3:56:**
```roc
import http.Client as Http exposing [Request, Response]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_type_annotations.md:4:1:4:17:**
```roc
import json.Json
```
^^^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_type_annotations.md:5:1:5:38:**
```roc
import utils.Result exposing [Result]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_type_annotations.md:17:9:17:23:**
```roc
        Ok(data) => Ok(Http.success(data))
```
        ^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_type_annotations.md:26:18:26:22:**
```roc
advancedParser : Json.Parser.Config, Str -> Result(Json.Value, Json.Parser.Error)
```
                 ^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_type_annotations.md:26:22:26:29:**
```roc
advancedParser : Json.Parser.Config, Str -> Result(Json.Value, Json.Parser.Error)
```
                     ^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_type_annotations.md:26:64:26:68:**
```roc
advancedParser : Json.Parser.Config, Str -> Result(Json.Value, Json.Parser.Error)
```
                                                               ^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_type_annotations.md:26:68:26:75:**
```roc
advancedParser : Json.Parser.Config, Str -> Result(Json.Value, Json.Parser.Error)
```
                                                                   ^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_type_annotations.md:27:40:27:44:**
```roc
advancedParser = |parserConfig, input| Json.Parser.parseWith(parserConfig, input)
```
                                       ^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_type_annotations.md:27:44:27:51:**
```roc
advancedParser = |parserConfig, input| Json.Parser.parseWith(parserConfig, input)
```
                                           ^^^^^^^


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
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "config")
    (Expr.module_access
      (Expr.malformed)
      (Expr.malformed)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "config")
    (Expr.module_access
      (Expr.malformed)
      (Expr.malformed)
    )
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
  (Expr.apply_tag)
  (Expr.malformed)
  (Expr.apply_tag)
  (Expr.malformed)
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

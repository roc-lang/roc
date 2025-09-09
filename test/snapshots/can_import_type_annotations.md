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
KwModule OpenSquare CloseSquare BlankLine KwImport LowerIdent Dot UpperIdent KwAs UpperIdent KwExposing OpenSquare UpperIdent Comma UpperIdent CloseSquare KwImport LowerIdent Dot UpperIdent KwImport LowerIdent Dot UpperIdent KwExposing OpenSquare UpperIdent CloseSquare BlankLine LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar UpperIdent Dot LowerIdent BlankLine LowerIdent OpColon UpperIdent OpArrow UpperIdent Dot UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound BlankLine LowerIdent OpColon UpperIdent Dot UpperIdent OpArrow UpperIdent OpenRound UpperIdent Dot UpperIdent Comma UpperIdent Dot UpperIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly LowerIdent OpAssign UpperIdent Dot LowerIdent OpenRound LowerIdent Dot LowerIdent CloseRound KwMatch LowerIdent OpenCurly UpperIdent OpenRound LowerIdent CloseRound OpThinArrow UpperIdent OpenRound UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound CloseRound UpperIdent OpenRound LowerIdent CloseRound OpThinArrow UpperIdent OpenRound LowerIdent CloseRound CloseCurly CloseCurly BlankLine LowerIdent OpColon UpperIdent Dot UpperIdent LowerIdent OpAssign UpperIdent Dot LowerIdent BlankLine LineComment LowerIdent OpColon UpperIdent Dot UpperIdent Dot UpperIdent Comma UpperIdent OpArrow UpperIdent OpenRound UpperIdent Dot UpperIdent Comma UpperIdent Dot UpperIdent Dot UpperIdent CloseRound LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar UpperIdent Dot UpperIdent Dot LowerIdent OpenRound LowerIdent Comma LowerIdent CloseRound BlankLine LineComment LowerIdent OpColon UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound Comma UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpArrow UpperIdent OpenRound OpenRound LowerIdent Comma LowerIdent CloseRound Comma LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar KwMatch LowerIdent OpenCurly UpperIdent OpenRound LowerIdent CloseRound OpThinArrow KwMatch OpenRound LowerIdent CloseRound OpenCurly UpperIdent OpenRound LowerIdent CloseRound OpThinArrow UpperIdent OpenRound OpenRound LowerIdent Comma LowerIdent CloseRound CloseRound UpperIdent OpenRound LowerIdent CloseRound OpThinArrow UpperIdent OpenRound LowerIdent CloseRound CloseCurly UpperIdent OpenRound LowerIdent CloseRound OpThinArrow UpperIdent OpenRound LowerIdent CloseRound CloseCurly ~~~
# PARSE
~~~clojure
(module-header)
(block
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
      )
    )
  )
  (import
    (binop_dot
      (lc "json")
      (uc "Json")
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
    (lc "processRequest")
    (binop_arrow_call
      (uc "Request")
      (uc "Response")
    )
  )
  (binop_equals
    (lc "processRequest")
    (lambda
      (body
        (binop_dot
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
    (binop_arrow_call
      (uc "Str")
      (binop_dot
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
    (lc "handleApi")
    (binop_arrow_call
      (binop_dot
        (uc "Http")
        (uc "Request")
      )
      (apply_uc
        (uc "Result")
        (tuple_literal
          (binop_dot
            (uc "Http")
            (uc "Response")
          )
          (binop_dot
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
              (binop_dot
                (uc "Json")
                (dot_lc "decode")
              )
              (binop_dot
                (lc "request")
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
                    (lc "data")
                  )
                  (malformed)
                )
))
            (lc "data")
          )
          (malformed)
          (apply_uc
            (uc "Err")
            (lc "err")
          )
          (malformed)
          (apply_uc
            (uc "Err")
            (lc "err")
          )
        )
      )
      (args
        (lc "request")
      )
    )
  )
  (malformed)
  (binop_colon
    (lc "config")
    (binop_dot
      (uc "Json")
      (uc "Config")
    )
  )
  (binop_equals
    (lc "config")
    (binop_dot
      (uc "Json")
      (dot_lc "defaultConfig")
    )
  )
  (binop_colon
    (lc "advancedParser")
    (binop_arrow_call
      (binop_dot
        (binop_dot
          (uc "Json")
          (uc "Parser")
        )
        (uc "Config")
      )
      (binop_arrow_call
        (uc "Str")
        (apply_uc
          (uc "Result")
          (tuple_literal
            (binop_dot
              (uc "Json")
              (uc "Value")
            )
            (binop_dot
              (binop_dot
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
          (binop_dot
            (binop_dot
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
        (lc "parserConfig")
        (lc "input")
      )
    )
  )
  (binop_colon
    (lc "combineResults")
    (binop_arrow_call
      (apply_uc
        (uc "Result")
        (tuple_literal
          (lc "a")
          (lc "err")
        )
      )
      (binop_arrow_call
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
)
          (branch1             (binop_thick_arrow
              (apply_uc
                (uc "Ok")
                (lc "value1")
              )
              (binop_thick_arrow
                (apply_anon
                  (match
                    (scrutinee                       (lc "result2")
)
                    (branch1                       (binop_thick_arrow
                        (apply_uc
                          (uc "Ok")
                          (lc "value2")
                        )
                        (malformed)
                      )
))
                  (lc "err")
                )
                (apply_uc
                  (uc "Err")
                  (lc "err")
                )
              )
            )
))
      )
      (args
        (lc "result1")
        (lc "result2")
      )
    )
  )
  (apply_uc
    (uc "Err")
    (lc "err")
  )
  (malformed)
  (apply_uc
    (uc "Err")
    (lc "err")
  )
  (malformed)
)
~~~
# FORMATTED
~~~roc
module []

import (http.Client) as Http exposing [Request, Response]
import json.Json
import utils.Result exposing [Result]
processRequest : Request -> Response
processRequest = |req| Http..defaultResponse
parseJson : Str -> Json.Value
parseJson = |input| Json..parse(input)
handleApi : Http.Request -> Result(Http.Response, Json.Error)
handleApi = |request| {
	result = Json..decode(request..body)
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
config = (Json..defaultConfig)
# Test nested type qualification
advancedParser : Json.Parser.Config -> Str -> Result(Json.Value, Json.Parser.Error)
advancedParser = |parserConfig, input| Json.Parser..parseWith((parserConfig, input))
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


**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**can_import_type_annotations.md:17:23:17:36:**
```roc
        Ok(data) => Ok(Http.success(data))
```
                      ^^^^^^^^^^^^^


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


**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**can_import_type_annotations.md:35:33:36:20:**
```roc
                Ok(value2) => Ok((value1, value2))
                Err(err) => Err(err)
```


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


**SHADOWING**
This definition shadows an existing one.

**can_import_type_annotations.md:7:1:7:15:**
```roc
processRequest : Request -> Response
```
^^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**can_import_type_annotations.md:8:1:8:15:**
```roc
processRequest = |req| Http.defaultResponse
```
^^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**can_import_type_annotations.md:10:1:10:10:**
```roc
parseJson : Str -> Json.Value
```
^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**can_import_type_annotations.md:11:1:11:10:**
```roc
parseJson = |input| Json.parse(input)
```
^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**can_import_type_annotations.md:13:1:13:10:**
```roc
handleApi : Http.Request -> Result(Http.Response, Json.Error)
```
^^^^^^^^^


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


**SHADOWING**
This definition shadows an existing one.

**can_import_type_annotations.md:14:1:14:10:**
```roc
handleApi = |request| {
```
^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**can_import_type_annotations.md:22:1:22:7:**
```roc
config : Json.Config
```
^^^^^^


**SHADOWING**
This definition shadows an existing one.

**can_import_type_annotations.md:23:1:23:7:**
```roc
config = Json.defaultConfig
```
^^^^^^


**SHADOWING**
This definition shadows an existing one.

**can_import_type_annotations.md:26:1:26:15:**
```roc
advancedParser : Json.Parser.Config, Str -> Result(Json.Value, Json.Parser.Error)
```
^^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**can_import_type_annotations.md:27:33:27:38:**
```roc
advancedParser = |parserConfig, input| Json.Parser.parseWith(parserConfig, input)
```
                                ^^^^^


**SHADOWING**
This definition shadows an existing one.

**can_import_type_annotations.md:27:1:27:15:**
```roc
advancedParser = |parserConfig, input| Json.Parser.parseWith(parserConfig, input)
```
^^^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**can_import_type_annotations.md:30:1:30:15:**
```roc
combineResults : Result(a, err), Result(b, err) -> Result((a, b), err)
```
^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_type_annotations.md:34:13:36:25:**
```roc
            match(result2) {
                Ok(value2) => Ok((value1, value2))
                Err(err) => Err(err)
```


**UNDEFINED VARIABLE**
Nothing is named **err** in this scope.
Is there an **import** or **exposing** missing up-top?

**can_import_type_annotations.md:36:33:36:36:**
```roc
                Err(err) => Err(err)
```
                                ^^^


**SHADOWING**
This definition shadows an existing one.

**can_import_type_annotations.md:31:1:31:15:**
```roc
combineResults = |result1, result2|
```
^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **err** in this scope.
Is there an **import** or **exposing** missing up-top?

**can_import_type_annotations.md:38:13:38:16:**
```roc
        Err(err) => Err(err)
```
            ^^^


**UNDEFINED VARIABLE**
Nothing is named **err** in this scope.
Is there an **import** or **exposing** missing up-top?

**can_import_type_annotations.md:38:25:38:28:**
```roc
        Err(err) => Err(err)
```
                        ^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
  (Stmt.import)
  (Stmt.import)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "processRequest"))
    (type type_25)
  )
  (Stmt.assign
    (pattern (Patt.ident "processRequest"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "parseJson"))
    (type type_39)
  )
  (Stmt.assign
    (pattern (Patt.ident "parseJson"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "handleApi"))
    (type type_63)
  )
  (Stmt.assign
    (pattern (Patt.ident "handleApi"))
    (Expr.lambda (canonicalized))
  )
  (Expr.malformed)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "config"))
    (type type_103)
  )
  (Stmt.assign
    (pattern (Patt.ident "config"))
    (Expr.record_access)
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "advancedParser"))
    (type type_129)
  )
  (Stmt.assign
    (pattern (Patt.ident "advancedParser"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "combineResults"))
    (type type_164)
  )
  (Stmt.assign
    (pattern (Patt.ident "combineResults"))
    (Expr.lambda (canonicalized))
  )
  (Expr.tag_applied)
  (Expr.malformed)
  (Expr.tag_applied)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 232
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
(var #27 -> #204)
(var #28 _)
(var #29 _)
(var #30 _)
(var #31 _)
(var #32 -> #204)
(var #33 _)
(var #34 _)
(var #35 _)
(var #36 _)
(var #37 _)
(var #38 _)
(var #39 _)
(var #40 _)
(var #41 -> #207)
(var #42 _)
(var #43 _)
(var #44 _)
(var #45 -> #206)
(var #46 _)
(var #47 _)
(var #48 -> #207)
(var #49 _)
(var #50 _)
(var #51 _)
(var #52 _)
(var #53 _)
(var #54 _)
(var #55 _)
(var #56 _)
(var #57 _)
(var #58 _)
(var #59 _)
(var #60 _)
(var #61 _)
(var #62 _)
(var #63 _)
(var #64 _)
(var #65 -> #215)
(var #66 _)
(var #67 -> #74)
(var #68 _)
(var #69 _)
(var #70 -> #209)
(var #71 _)
(var #72 _)
(var #73 _)
(var #74 _)
(var #75 _)
(var #76 _)
(var #77 _)
(var #78 _)
(var #79 _)
(var #80 _)
(var #81 _)
(var #82 _)
(var #83 _)
(var #84 _)
(var #85 -> #210)
(var #86 _)
(var #87 _)
(var #88 _)
(var #89 -> #212)
(var #90 _)
(var #91 _)
(var #92 _)
(var #93 -> #214)
(var #94 _)
(var #95 _)
(var #96 _)
(var #97 -> #215)
(var #98 _)
(var #99 _)
(var #100 _)
(var #101 _)
(var #102 _)
(var #103 _)
(var #104 _)
(var #105 -> #108)
(var #106 _)
(var #107 _)
(var #108 _)
(var #109 _)
(var #110 _)
(var #111 _)
(var #112 _)
(var #113 _)
(var #114 _)
(var #115 _)
(var #116 _)
(var #117 _)
(var #118 _)
(var #119 _)
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
(var #131 -> #223)
(var #132 _)
(var #133 _)
(var #134 _)
(var #135 _)
(var #136 -> #219)
(var #137 _)
(var #138 -> #221)
(var #139 _)
(var #140 _)
(var #141 -> #220)
(var #142 _)
(var #143 -> #223)
(var #144 _)
(var #145 _)
(var #146 _)
(var #147 _)
(var #148 _)
(var #149 _)
(var #150 _)
(var #151 _)
(var #152 _)
(var #153 _)
(var #154 _)
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
(var #166 -> #227)
(var #167 _)
(var #168 _)
(var #169 _)
(var #170 _)
(var #171 _)
(var #172 _)
(var #173 _)
(var #174 _)
(var #175 _)
(var #176 _)
(var #177 _)
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
(var #192 -> #227)
(var #193 _)
(var #194 -> #228)
(var #195 _)
(var #196 _)
(var #197 _)
(var #198 -> #230)
(var #199 _)
(var #200 _)
(var #201 _)
(var #202 _)
(var #203 _)
(var #204 fn_pure)
(var #205 _)
(var #206 fn_pure)
(var #207 fn_pure)
(var #208 _)
(var #209 fn_pure)
(var #210 fn_pure)
(var #211 _)
(var #212 fn_pure)
(var #213 _)
(var #214 fn_pure)
(var #215 fn_pure)
(var #216 _)
(var #217 _)
(var #218 _)
(var #219 _)
(var #220 tuple)
(var #221 fn_pure)
(var #222 fn_pure)
(var #223 fn_pure)
(var #224 _)
(var #225 _)
(var #226 fn_pure)
(var #227 fn_pure)
(var #228 fn_pure)
(var #229 _)
(var #230 fn_pure)
(var #231 _)
~~~
# TYPES
~~~roc
parserConfig : _c
processRequest : _arg -> _ret
config : _c
parseJson : _arg -> _ret
combineResults : _arg -> _arg2 -> _ret
req : _c
result2 : _c
result1 : _c
handleApi : _arg -> _ret
advancedParser : _arg -> _arg2 -> _ret
value1 : _c
data : _c
request : _c
input : _c
~~~

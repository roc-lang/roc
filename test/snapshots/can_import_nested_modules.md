# META
~~~ini
description=Nested module qualification
type=file
~~~
# SOURCE
~~~roc
module []

import json.Parser.Config
import http.Client.Auth as HttpAuth
import utils.String.Format exposing [padLeft]

# Test multi-level type qualification
parseConfig : Config.Settings -> Str
parseConfig = |settings| Config.toString(settings)

# Test multi-level value qualification
authenticate : Str, Str -> HttpAuth.Token
authenticate = |user, pass| HttpAuth.login(user, pass)

# Test deeply nested qualification
processData : Config.Parser.Advanced, Str -> Result(Str, Config.Parser.Error)
processData = |advancedConfig, input|
    Config.Parser.Advanced.parseWith(advancedConfig, input)

# Test mixed qualification (exposed item + qualified)
formatOutput : Str -> Str
formatOutput = |text| padLeft(text, Config.defaultPadding)

# Test qualified type in function signature
validateAuth : HttpAuth.Credentials -> Result(HttpAuth.Token, HttpAuth.Error)
validateAuth = |creds| HttpAuth.validate(creds)
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine KwImport LowerIdent Dot UpperIdent Dot UpperIdent KwImport LowerIdent Dot UpperIdent Dot UpperIdent KwAs UpperIdent KwImport LowerIdent Dot UpperIdent Dot UpperIdent KwExposing OpenSquare LowerIdent CloseSquare BlankLine LineComment LowerIdent OpColon UpperIdent Dot UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound BlankLine LineComment LowerIdent OpColon UpperIdent Comma UpperIdent OpArrow UpperIdent Dot UpperIdent LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar UpperIdent Dot LowerIdent OpenRound LowerIdent Comma LowerIdent CloseRound BlankLine LineComment LowerIdent OpColon UpperIdent Dot UpperIdent Dot UpperIdent Comma UpperIdent OpArrow UpperIdent OpenRound UpperIdent Comma UpperIdent Dot UpperIdent Dot UpperIdent CloseRound LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar UpperIdent Dot UpperIdent Dot UpperIdent Dot LowerIdent OpenRound LowerIdent Comma LowerIdent CloseRound BlankLine LineComment LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpenRound LowerIdent Comma UpperIdent Dot LowerIdent CloseRound BlankLine LineComment LowerIdent OpColon UpperIdent Dot UpperIdent OpArrow UpperIdent OpenRound UpperIdent Dot UpperIdent Comma UpperIdent Dot UpperIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound ~~~
# PARSE
~~~clojure
(module-header)
(block
  (import
    (binop_pipe
      (binop_pipe
        (lc "json")
        (uc "Parser")
      )
      (uc "Config")
    )
  )
  (import
    (binop_as
      (binop_pipe
        (binop_pipe
          (lc "http")
          (uc "Client")
        )
        (uc "Auth")
      )
      (uc "HttpAuth")
    )
  )
  (import
    (binop_exposing
      (binop_pipe
        (binop_pipe
          (lc "utils")
          (uc "String")
        )
        (uc "Format")
      )
      (list_literal
        (lc "padLeft")
      )
    )
  )
  (binop_colon
    (lc "parseConfig")
    (binop_arrow_call
      (binop_pipe
        (uc "Config")
        (uc "Settings")
      )
      (uc "Str")
    )
  )
  (binop_equals
    (lc "parseConfig")
    (lambda
      (body
        (apply_anon
          (binop_pipe
            (uc "Config")
            (dot_lc "toString")
          )
          (lc "settings")
        )
      )
      (args
        (lc "settings")
      )
    )
  )
  (binop_colon
    (lc "authenticate")
    (binop_arrow_call
      (uc "Str")
      (binop_arrow_call
        (uc "Str")
        (binop_pipe
          (uc "HttpAuth")
          (uc "Token")
        )
      )
    )
  )
  (binop_equals
    (lc "authenticate")
    (lambda
      (body
        (apply_anon
          (binop_pipe
            (uc "HttpAuth")
            (dot_lc "login")
          )
          (tuple_literal
            (lc "user")
            (lc "pass")
          )
        )
      )
      (args
        (lc "user")
        (lc "pass")
      )
    )
  )
  (binop_colon
    (lc "processData")
    (binop_arrow_call
      (binop_pipe
        (binop_pipe
          (uc "Config")
          (uc "Parser")
        )
        (uc "Advanced")
      )
      (binop_arrow_call
        (uc "Str")
        (apply_uc
          (uc "Result")
          (tuple_literal
            (uc "Str")
            (binop_pipe
              (binop_pipe
                (uc "Config")
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
    (lc "processData")
    (lambda
      (body
        (apply_anon
          (binop_pipe
            (binop_pipe
              (binop_pipe
                (uc "Config")
                (uc "Parser")
              )
              (uc "Advanced")
            )
            (dot_lc "parseWith")
          )
          (tuple_literal
            (lc "advancedConfig")
            (lc "input")
          )
        )
      )
      (args
        (lc "advancedConfig")
        (lc "input")
      )
    )
  )
  (binop_colon
    (lc "formatOutput")
    (binop_arrow_call
      (uc "Str")
      (uc "Str")
    )
  )
  (binop_equals
    (lc "formatOutput")
    (lambda
      (body
        (apply_lc
          (lc "padLeft")
          (tuple_literal
            (lc "text")
            (binop_pipe
              (uc "Config")
              (dot_lc "defaultPadding")
            )
          )
        )
      )
      (args
        (lc "text")
      )
    )
  )
  (binop_colon
    (lc "validateAuth")
    (binop_arrow_call
      (binop_pipe
        (uc "HttpAuth")
        (uc "Credentials")
      )
      (apply_uc
        (uc "Result")
        (tuple_literal
          (binop_pipe
            (uc "HttpAuth")
            (uc "Token")
          )
          (binop_pipe
            (uc "HttpAuth")
            (uc "Error")
          )
        )
      )
    )
  )
  (binop_equals
    (lc "validateAuth")
    (lambda
      (body
        (apply_anon
          (binop_pipe
            (uc "HttpAuth")
            (dot_lc "validate")
          )
          (lc "creds")
        )
      )
      (args
        (lc "creds")
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
module []

import json.Parser | Config
import http.Client | Auth as HttpAuth
import utils.String | Format exposing [padLeft]
# Test multi-level type qualification
parseConfig : Config.Settings -> Str
parseConfig = |settings| Config.toString(settings)
# Test multi-level value qualification
authenticate : Str -> Str -> HttpAuth.Token
authenticate = |user, pass| HttpAuth.login((user, pass))
# Test deeply nested qualification
processData : Config.Parser | Advanced -> Str -> Result(Str, Config.Parser | Error)
processData = |advancedConfig, input| Config.Parser | Advanced | .parseWith((advancedConfig, input))
# Test mixed qualification (exposed item + qualified)
formatOutput : Str -> Str
formatOutput = |text| padLeft((text, Config.defaultPadding))
# Test qualified type in function signature
validateAuth : HttpAuth.Credentials -> Result(HttpAuth.Token, HttpAuth.Error)
validateAuth = |creds| HttpAuth.validate(creds)
~~~
# EXPECTED
PARSE ERROR - can_import_nested_modules.md:3:19:3:26
PARSE ERROR - can_import_nested_modules.md:4:19:4:24
PARSE ERROR - can_import_nested_modules.md:4:25:4:27
PARSE ERROR - can_import_nested_modules.md:5:1:5:7
PARSE ERROR - can_import_nested_modules.md:5:8:5:13
PARSE ERROR - can_import_nested_modules.md:5:13:5:20
PARSE ERROR - can_import_nested_modules.md:5:20:5:27
PARSE ERROR - can_import_nested_modules.md:5:28:5:36
PARSE ERROR - can_import_nested_modules.md:5:37:5:38
PARSE ERROR - can_import_nested_modules.md:5:38:5:45
PARSE ERROR - can_import_nested_modules.md:5:45:5:46
MODULE NOT FOUND - can_import_nested_modules.md:3:1:3:19
MODULE NOT FOUND - can_import_nested_modules.md:4:1:4:19
MODULE NOT IMPORTED - can_import_nested_modules.md:8:15:8:30
UNDEFINED VARIABLE - can_import_nested_modules.md:9:26:9:41
MODULE NOT IMPORTED - can_import_nested_modules.md:12:28:12:42
UNDEFINED VARIABLE - can_import_nested_modules.md:13:29:13:43
MODULE NOT IMPORTED - can_import_nested_modules.md:16:15:16:37
MODULE NOT IMPORTED - can_import_nested_modules.md:16:58:16:77
UNDEFINED VARIABLE - can_import_nested_modules.md:18:5:18:37
UNDEFINED VARIABLE - can_import_nested_modules.md:22:23:22:30
UNDEFINED VARIABLE - can_import_nested_modules.md:22:37:22:58
MODULE NOT IMPORTED - can_import_nested_modules.md:25:16:25:36
MODULE NOT IMPORTED - can_import_nested_modules.md:25:47:25:61
MODULE NOT IMPORTED - can_import_nested_modules.md:25:63:25:77
UNDEFINED VARIABLE - can_import_nested_modules.md:26:24:26:41
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **json** in this scope.
Is there an **import** or **exposing** missing up-top?

**can_import_nested_modules.md:3:8:3:12:**
```roc
import json.Parser.Config
```
       ^^^^


**UNDEFINED VARIABLE**
Nothing is named **utils** in this scope.
Is there an **import** or **exposing** missing up-top?

**can_import_nested_modules.md:5:8:5:13:**
```roc
import utils.String.Format exposing [padLeft]
```
       ^^^^^


**EXPRESSION IN TYPE CONTEXT**
Found an expression where a type was expected.
Types must be type identifiers, type applications, or type expressions.

**can_import_nested_modules.md:8:15:8:30:**
```roc
parseConfig : Config.Settings -> Str
```
              ^^^^^^^^^^^^^^^


**EXPRESSION IN TYPE CONTEXT**
Found an expression where a type was expected.
Types must be type identifiers, type applications, or type expressions.

**can_import_nested_modules.md:12:28:12:42:**
```roc
authenticate : Str, Str -> HttpAuth.Token
```
                           ^^^^^^^^^^^^^^


**EXPRESSION IN TYPE CONTEXT**
Found an expression where a type was expected.
Types must be type identifiers, type applications, or type expressions.

**can_import_nested_modules.md:16:15:16:37:**
```roc
processData : Config.Parser.Advanced, Str -> Result(Str, Config.Parser.Error)
```
              ^^^^^^^^^^^^^^^^^^^^^^


**EXPRESSION IN TYPE CONTEXT**
Found an expression where a type was expected.
Types must be type identifiers, type applications, or type expressions.

**can_import_nested_modules.md:16:58:16:77:**
```roc
processData : Config.Parser.Advanced, Str -> Result(Str, Config.Parser.Error)
```
                                                         ^^^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **padLeft** in this scope.
Is there an **import** or **exposing** missing up-top?

**can_import_nested_modules.md:22:23:22:30:**
```roc
formatOutput = |text| padLeft(text, Config.defaultPadding)
```
                      ^^^^^^^


**EXPRESSION IN TYPE CONTEXT**
Found an expression where a type was expected.
Types must be type identifiers, type applications, or type expressions.

**can_import_nested_modules.md:25:16:25:36:**
```roc
validateAuth : HttpAuth.Credentials -> Result(HttpAuth.Token, HttpAuth.Error)
```
               ^^^^^^^^^^^^^^^^^^^^


**EXPRESSION IN TYPE CONTEXT**
Found an expression where a type was expected.
Types must be type identifiers, type applications, or type expressions.

**can_import_nested_modules.md:25:47:25:61:**
```roc
validateAuth : HttpAuth.Credentials -> Result(HttpAuth.Token, HttpAuth.Error)
```
                                              ^^^^^^^^^^^^^^


**EXPRESSION IN TYPE CONTEXT**
Found an expression where a type was expected.
Types must be type identifiers, type applications, or type expressions.

**can_import_nested_modules.md:25:63:25:77:**
```roc
validateAuth : HttpAuth.Credentials -> Result(HttpAuth.Token, HttpAuth.Error)
```
                                                              ^^^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
  (Stmt.import)
  (Stmt.import)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "parseConfig"))
    (type type_29)
  )
  (Stmt.assign
    (pattern (Patt.ident "parseConfig"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "authenticate"))
    (type type_47)
  )
  (Stmt.assign
    (pattern (Patt.ident "authenticate"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "processData"))
    (type type_78)
  )
  (Stmt.assign
    (pattern (Patt.ident "processData"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "formatOutput"))
    (type type_99)
  )
  (Stmt.assign
    (pattern (Patt.ident "formatOutput"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "validateAuth"))
    (type type_125)
  )
  (Stmt.assign
    (pattern (Patt.ident "validateAuth"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 157
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
(var #31 -> #139)
(var #32 _)
(var #33 _)
(var #34 _)
(var #35 -> #138)
(var #36 _)
(var #37 _)
(var #38 -> #139)
(var #39 _)
(var #40 _)
(var #41 _)
(var #42 _)
(var #43 _)
(var #44 _)
(var #45 _)
(var #46 _)
(var #47 _)
(var #48 _)
(var #49 -> #144)
(var #50 _)
(var #51 _)
(var #52 _)
(var #53 _)
(var #54 -> #143)
(var #55 _)
(var #56 _)
(var #57 -> #142)
(var #58 _)
(var #59 -> #144)
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
(var #70 _)
(var #71 _)
(var #72 _)
(var #73 _)
(var #74 _)
(var #75 _)
(var #76 _)
(var #77 _)
(var #78 _)
(var #79 _)
(var #80 -> #149)
(var #81 _)
(var #82 _)
(var #83 _)
(var #84 _)
(var #85 _)
(var #86 _)
(var #87 _)
(var #88 _)
(var #89 -> #148)
(var #90 _)
(var #91 _)
(var #92 -> #147)
(var #93 _)
(var #94 -> #149)
(var #95 _)
(var #96 _)
(var #97 _)
(var #98 _)
(var #99 _)
(var #100 _)
(var #101 -> #153)
(var #102 _)
(var #103 -> #152)
(var #104 _)
(var #105 _)
(var #106 _)
(var #107 _)
(var #108 -> #151)
(var #109 _)
(var #110 -> #153)
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
(var #127 -> #156)
(var #128 _)
(var #129 _)
(var #130 _)
(var #131 -> #155)
(var #132 _)
(var #133 _)
(var #134 -> #156)
(var #135 _)
(var #136 _)
(var #137 _)
(var #138 fn_pure)
(var #139 fn_pure)
(var #140 _)
(var #141 _)
(var #142 tuple)
(var #143 fn_pure)
(var #144 fn_pure)
(var #145 _)
(var #146 _)
(var #147 tuple)
(var #148 fn_pure)
(var #149 fn_pure)
(var #150 _)
(var #151 tuple)
(var #152 fn_pure)
(var #153 fn_pure)
(var #154 _)
(var #155 fn_pure)
(var #156 fn_pure)
~~~
# TYPES
~~~roc
settings : _a
validateAuth : _arg -> _ret
creds : _a
formatOutput : _arg -> _ret
user : _a
processData : _arg, _arg2 -> _ret
text : _a
advancedConfig : _a
authenticate : _arg, _arg2 -> _ret
pass : _a
input : _a
parseConfig : _arg -> _ret
~~~

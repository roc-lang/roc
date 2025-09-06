# META
~~~ini
description=Error handling for unresolved qualified names
type=file
~~~
# SOURCE
~~~roc
module []

import json.Json
import http.Client as Http

# Test unresolved qualified value
main = Json.NonExistent.method

# Test unresolved qualified type in annotation
parseData : Json.InvalidType -> Str
parseData = |data| Json.stringify(data)

# Test unresolved nested qualification
processRequest : Http.Server.Request -> Http.Server.Response
processRequest = |req| Http.Server.defaultResponse

# Test typo in qualified name
result = Json.prase("test")

# Test unknown module qualification
config = Unknown.Module.config

# Test valid module but invalid member
client = Http.invalidMethod

# Test deeply nested invalid qualification
parser = Json.Parser.Advanced.NonExistent.create
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine KwImport LowerIdent Dot UpperIdent KwImport LowerIdent Dot UpperIdent KwAs UpperIdent BlankLine LineComment LowerIdent OpAssign UpperIdent Dot UpperIdent Dot LowerIdent BlankLine LineComment LowerIdent OpColon UpperIdent Dot UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound BlankLine LineComment LowerIdent OpColon UpperIdent Dot UpperIdent Dot UpperIdent OpArrow UpperIdent Dot UpperIdent Dot UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar UpperIdent Dot UpperIdent Dot LowerIdent BlankLine LineComment LowerIdent OpAssign UpperIdent Dot LowerIdent OpenRound String CloseRound BlankLine LineComment LowerIdent OpAssign UpperIdent Dot UpperIdent Dot LowerIdent BlankLine LineComment LowerIdent OpAssign UpperIdent Dot LowerIdent BlankLine LineComment LowerIdent OpAssign UpperIdent Dot UpperIdent Dot UpperIdent Dot UpperIdent Dot LowerIdent ~~~
# PARSE
~~~clojure
(module-header)
(block
  (import
    (binop_pipe
      (lc "json")
      (uc "Json")
    )
  )
  (import
    (binop_as
      (binop_pipe
        (lc "http")
        (uc "Client")
      )
      (uc "Http")
    )
  )
  (binop_equals
    (lc "main")
    (binop_pipe
      (binop_pipe
        (uc "Json")
        (uc "NonExistent")
      )
      (dot_lc "method")
    )
  )
  (binop_colon
    (lc "parseData")
    (binop_arrow_call
      (binop_pipe
        (uc "Json")
        (uc "InvalidType")
      )
      (uc "Str")
    )
  )
  (binop_equals
    (lc "parseData")
    (lambda
      (body
        (apply_anon
          (binop_pipe
            (uc "Json")
            (dot_lc "stringify")
          )
          (lc "data")
        )
      )
      (args
        (lc "data")
      )
    )
  )
  (binop_colon
    (lc "processRequest")
    (binop_arrow_call
      (binop_pipe
        (binop_pipe
          (uc "Http")
          (uc "Server")
        )
        (uc "Request")
      )
      (binop_pipe
        (binop_pipe
          (uc "Http")
          (uc "Server")
        )
        (uc "Response")
      )
    )
  )
  (binop_equals
    (lc "processRequest")
    (lambda
      (body
        (binop_pipe
          (binop_pipe
            (uc "Http")
            (uc "Server")
          )
          (dot_lc "defaultResponse")
        )
      )
      (args
        (lc "req")
      )
    )
  )
  (binop_equals
    (lc "result")
    (apply_anon
      (binop_pipe
        (uc "Json")
        (dot_lc "prase")
      )
      (str_literal_small "test")
    )
  )
  (binop_equals
    (lc "config")
    (binop_pipe
      (binop_pipe
        (uc "Unknown")
        (uc "Module")
      )
      (dot_lc "config")
    )
  )
  (binop_equals
    (lc "client")
    (binop_pipe
      (uc "Http")
      (dot_lc "invalidMethod")
    )
  )
  (binop_equals
    (lc "parser")
    (binop_pipe
      (binop_pipe
        (binop_pipe
          (binop_pipe
            (uc "Json")
            (uc "Parser")
          )
          (uc "Advanced")
        )
        (uc "NonExistent")
      )
      (dot_lc "create")
    )
  )
)
~~~
# FORMATTED
~~~roc
module []

import json.Json
import http.Client as Http
# Test unresolved qualified value
main = (Json.NonExistent | .method)
# Test unresolved qualified type in annotation
parseData : Json.InvalidType -> Str
parseData = |data| Json.stringify(data)
# Test unresolved nested qualification
processRequest : Http.Server | Request -> Http.Server | Response
processRequest = |req| Http.Server | .defaultResponse
# Test typo in qualified name
result = Json.prase("test")
# Test unknown module qualification
config = (Unknown.Module | .config)
# Test valid module but invalid member
client = Http.invalidMethod
# Test deeply nested invalid qualification
parser = (Json.Parser | Advanced | NonExistent | .create)
~~~
# EXPECTED
MODULE NOT FOUND - can_import_unresolved_qualified.md:3:1:3:17
MODULE NOT FOUND - can_import_unresolved_qualified.md:4:1:4:27
MODULE NOT IMPORTED - can_import_unresolved_qualified.md:14:18:14:37
MODULE NOT IMPORTED - can_import_unresolved_qualified.md:14:41:14:61
UNUSED VARIABLE - can_import_unresolved_qualified.md:15:19:15:22
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **json** in this scope.
Is there an **import** or **exposing** missing up-top?

**can_import_unresolved_qualified.md:3:8:3:12:**
```roc
import json.Json
```
       ^^^^


**EXPRESSION IN TYPE CONTEXT**
Found an expression where a type was expected.
Types must be type identifiers, type applications, or type expressions.

**can_import_unresolved_qualified.md:10:13:10:29:**
```roc
parseData : Json.InvalidType -> Str
```
            ^^^^^^^^^^^^^^^^


**EXPRESSION IN TYPE CONTEXT**
Found an expression where a type was expected.
Types must be type identifiers, type applications, or type expressions.

**can_import_unresolved_qualified.md:14:18:14:37:**
```roc
processRequest : Http.Server.Request -> Http.Server.Response
```
                 ^^^^^^^^^^^^^^^^^^^


**EXPRESSION IN TYPE CONTEXT**
Found an expression where a type was expected.
Types must be type identifiers, type applications, or type expressions.

**can_import_unresolved_qualified.md:14:41:14:61:**
```roc
processRequest : Http.Server.Request -> Http.Server.Response
```
                                        ^^^^^^^^^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
  (Stmt.import)
  (Stmt.assign
    (pattern (Patt.ident "main"))
    (Expr.binop_pipe)
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "parseData"))
    (type type_23)
  )
  (Stmt.assign
    (pattern (Patt.ident "parseData"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "processRequest"))
    (type type_45)
  )
  (Stmt.assign
    (pattern (Patt.ident "processRequest"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "result"))
    (Expr.fn_call)
  )
  (Stmt.assign
    (pattern (Patt.ident "config"))
    (Expr.binop_pipe)
  )
  (Stmt.assign
    (pattern (Patt.ident "client"))
    (Expr.binop_pipe)
  )
  (Stmt.assign
    (pattern (Patt.ident "parser"))
    (Expr.binop_pipe)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 93
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
(var #11 -> #16)
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
(var #25 -> #89)
(var #26 _)
(var #27 _)
(var #28 _)
(var #29 -> #88)
(var #30 _)
(var #31 _)
(var #32 -> #89)
(var #33 _)
(var #34 _)
(var #35 _)
(var #36 _)
(var #37 _)
(var #38 _)
(var #39 _)
(var #40 _)
(var #41 _)
(var #42 _)
(var #43 _)
(var #44 _)
(var #45 _)
(var #46 _)
(var #47 -> #91)
(var #48 _)
(var #49 _)
(var #50 _)
(var #51 _)
(var #52 _)
(var #53 _)
(var #54 -> #91)
(var #55 _)
(var #56 -> #61)
(var #57 _)
(var #58 _)
(var #59 -> #92)
(var #60 Str)
(var #61 _)
(var #62 _)
(var #63 -> #68)
(var #64 _)
(var #65 _)
(var #66 _)
(var #67 _)
(var #68 _)
(var #69 _)
(var #70 -> #73)
(var #71 _)
(var #72 _)
(var #73 _)
(var #74 _)
(var #75 -> #84)
(var #76 _)
(var #77 _)
(var #78 _)
(var #79 _)
(var #80 _)
(var #81 _)
(var #82 _)
(var #83 _)
(var #84 _)
(var #85 _)
(var #86 _)
(var #87 _)
(var #88 fn_pure)
(var #89 fn_pure)
(var #90 _)
(var #91 fn_pure)
(var #92 fn_pure)
~~~
# TYPES
~~~roc
main : _a
data : _a
result : _a
config : _a
processRequest : _arg -> _ret
client : _a
parser : _a
req : _a
parseData : _arg -> _ret
~~~

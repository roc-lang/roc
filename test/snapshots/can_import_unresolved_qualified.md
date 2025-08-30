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
KwModule OpenSquare CloseSquare KwImport LowerIdent Dot UpperIdent KwImport LowerIdent Dot UpperIdent KwAs UpperIdent LowerIdent OpAssign UpperIdent Dot UpperIdent Dot LowerIdent LowerIdent OpColon UpperIdent Dot UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound LowerIdent OpColon UpperIdent Dot UpperIdent Dot UpperIdent OpArrow UpperIdent Dot UpperIdent Dot UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar UpperIdent Dot UpperIdent Dot LowerIdent LowerIdent OpAssign UpperIdent Dot LowerIdent OpenRound String CloseRound LowerIdent OpAssign UpperIdent Dot UpperIdent Dot LowerIdent LowerIdent OpAssign UpperIdent Dot LowerIdent LowerIdent OpAssign UpperIdent Dot UpperIdent Dot UpperIdent Dot UpperIdent Dot LowerIdent ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
~~~roc
module []

import json.Json
import http.Client as Http
main = (Json.NonExistent | .method)
parseData : Json.InvalidType -> Str
parseData = |data| Json.stringify(data)
processRequest : Http.Server | Request -> Http.Server | Response
processRequest = |req| Http.Server | .defaultResponse
result = Json.prase("test")
config = (Unknown.Module | .config)
client = Http.invalidMethod
parser = (Json.Parser | Advanced | NonExistent | .create)
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 3:1 to 3:17

**Unsupported Node**
at 4:1 to 4:27

**Unsupported Node**
at 7:8 to 7:12

**Unsupported Node**
at 7:12 to 7:24

**Unsupported Node**
at 11:20 to 11:24

**Unsupported Node**
at 14:18 to 14:22

**Unsupported Node**
at 14:22 to 14:29

**Unsupported Node**
at 14:41 to 14:45

**Unsupported Node**
at 14:45 to 14:52

**Unsupported Node**
at 15:24 to 15:28

**Unsupported Node**
at 15:28 to 15:35

**Unsupported Node**
at 18:10 to 18:14

**Unsupported Node**
at 21:10 to 21:17

**Unsupported Node**
at 21:17 to 21:24

**Unsupported Node**
at 24:10 to 24:14

**Unsupported Node**
at 27:10 to 27:14

**Unsupported Node**
at 27:14 to 27:21

**Unsupported Node**
at 27:21 to 27:30

**Unsupported Node**
at 27:30 to 27:42

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.binop_equals
    (Expr.lookup "main")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.lookup "parseData")
    (Expr.binop_thin_arrow
      (Expr.module_access
        (Expr.malformed)
        (Expr.malformed)
      )
      (Expr.apply_tag)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "parseData")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.lookup "processRequest")
    (Expr.binop_thin_arrow
      (Expr.lambda)
      (Expr.lambda)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "processRequest")
    (Expr.lambda)
  )
  (Expr.binop_equals
    (Expr.lookup "result")
    (Expr.apply_ident)
  )
  (Expr.binop_equals
    (Expr.lookup "config")
    (Expr.lambda)
  )
  (Expr.binop_equals
    (Expr.lookup "client")
    (Expr.lambda)
  )
  (Expr.binop_equals
    (Expr.lookup "parser")
    (Expr.lambda)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
main : _a
parseData : _a
processRequest : _a
result : _a
config : _a
client : _a
parser : _a
~~~

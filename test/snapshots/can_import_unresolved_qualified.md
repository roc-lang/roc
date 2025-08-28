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
    (binop_thin_arrow
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
    (binop_thin_arrow
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
main = Json.NonExistent | .method
parseData : Json.InvalidType -> Str
parseData = |data| Json.stringify(data)
processRequest : Http.Server | Request -> Http.Server | Response
processRequest = |req| Http.Server | .defaultResponse
result = Json.prase("test")
config = Unknown.Module | .config
client = Http.invalidMethod
parser = ((Json.Parser | Advanced) | NonExistent) | .create
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.record_access)
~~~
# SOLVED
~~~clojure
(expr :tag record_access :type "_a")
~~~
# TYPES
~~~roc
# File does not contain a block of statements
~~~

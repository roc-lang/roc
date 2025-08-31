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
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **Json.stringify** in this scope.
Is there an **import** or **exposing** missing up-top?

**can_import_unresolved_qualified.md:11:20:11:34:**
```roc
parseData = |data| Json.stringify(data)
```
                   ^^^^^^^^^^^^^^


**UNUSED VARIABLE**
Variable **req** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_req` to suppress this warning.
The unused variable is declared here:

**can_import_unresolved_qualified.md:15:19:15:22:**
```roc
processRequest = |req| Http.Server.defaultResponse
```
                  ^^^


**UNDEFINED VARIABLE**
Nothing is named **Json.prase** in this scope.
Is there an **import** or **exposing** missing up-top?

**can_import_unresolved_qualified.md:18:10:18:20:**
```roc
result = Json.prase("test")
```
         ^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **Http.invalidMethod** in this scope.
Is there an **import** or **exposing** missing up-top?

**can_import_unresolved_qualified.md:24:10:24:28:**
```roc
client = Http.invalidMethod
```
         ^^^^^^^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~

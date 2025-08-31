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
main = (Json.NonExistent | .method)
parseData : Json.InvalidType -> Str
parseData = |data| Json.stringify(data)
processRequest : Http.Server | Request -> Http.Server | Response
processRequest = |req| Http.Server | .defaultResponse
result = Json.prase("test")
config = (Unknown.Module | .config)
client = Http.invalidMethod
parser = (Json.Parser | Advanced | NonExistent | .create)# Test unresolved qualified value
# Test unresolved qualified type in annotation
# Test unresolved nested qualification
# Test typo in qualified name
# Test unknown module qualification
# Test valid module but invalid member
# Test deeply nested invalid qualification
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_unresolved_qualified.md:3:1:3:17:**
```roc
import json.Json
```
^^^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_unresolved_qualified.md:4:1:4:27:**
```roc
import http.Client as Http
```
^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_unresolved_qualified.md:7:8:7:12:**
```roc
main = Json.NonExistent.method
```
       ^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_unresolved_qualified.md:7:12:7:24:**
```roc
main = Json.NonExistent.method
```
           ^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_unresolved_qualified.md:11:20:11:24:**
```roc
parseData = |data| Json.stringify(data)
```
                   ^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_unresolved_qualified.md:14:18:14:22:**
```roc
processRequest : Http.Server.Request -> Http.Server.Response
```
                 ^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_unresolved_qualified.md:14:22:14:29:**
```roc
processRequest : Http.Server.Request -> Http.Server.Response
```
                     ^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_unresolved_qualified.md:14:41:14:45:**
```roc
processRequest : Http.Server.Request -> Http.Server.Response
```
                                        ^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_unresolved_qualified.md:14:45:14:52:**
```roc
processRequest : Http.Server.Request -> Http.Server.Response
```
                                            ^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_unresolved_qualified.md:15:24:15:28:**
```roc
processRequest = |req| Http.Server.defaultResponse
```
                       ^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_unresolved_qualified.md:15:28:15:35:**
```roc
processRequest = |req| Http.Server.defaultResponse
```
                           ^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_unresolved_qualified.md:18:10:18:14:**
```roc
result = Json.prase("test")
```
         ^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_unresolved_qualified.md:21:10:21:17:**
```roc
config = Unknown.Module.config
```
         ^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_unresolved_qualified.md:21:17:21:24:**
```roc
config = Unknown.Module.config
```
                ^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_unresolved_qualified.md:24:10:24:14:**
```roc
client = Http.invalidMethod
```
         ^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_unresolved_qualified.md:27:10:27:14:**
```roc
parser = Json.Parser.Advanced.NonExistent.create
```
         ^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_unresolved_qualified.md:27:14:27:21:**
```roc
parser = Json.Parser.Advanced.NonExistent.create
```
             ^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_unresolved_qualified.md:27:21:27:30:**
```roc
parser = Json.Parser.Advanced.NonExistent.create
```
                    ^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_unresolved_qualified.md:27:30:27:42:**
```roc
parser = Json.Parser.Advanced.NonExistent.create
```
                             ^^^^^^^^^^^^


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

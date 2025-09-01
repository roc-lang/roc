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
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_unresolved_qualified.md:7:8:7:31:**
```roc
main = Json.NonExistent.method
```
       ^^^^^^^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **Json.stringify** in this scope.
Is there an **import** or **exposing** missing up-top?

**can_import_unresolved_qualified.md:11:20:11:34:**
```roc
parseData = |data| Json.stringify(data)
```
                   ^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_unresolved_qualified.md:15:24:15:51:**
```roc
processRequest = |req| Http.Server.defaultResponse
```
                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^


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


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_unresolved_qualified.md:21:10:21:31:**
```roc
config = Unknown.Module.config
```
         ^^^^^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **Http.invalidMethod** in this scope.
Is there an **import** or **exposing** missing up-top?

**can_import_unresolved_qualified.md:24:10:24:28:**
```roc
client = Http.invalidMethod
```
         ^^^^^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_unresolved_qualified.md:27:10:27:49:**
```roc
parser = Json.Parser.Advanced.NonExistent.create
```
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
  (Stmt.import)
  (Stmt.assign
    (pattern (Patt.ident "main"))
    (Expr.malformed)
  )
  (Stmt.type_anno
    (name "parseData")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "parseData"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_anno
    (name "processRequest")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "processRequest"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "result"))
    (Expr.apply_ident)
  )
  (Stmt.assign
    (pattern (Patt.ident "config"))
    (Expr.malformed)
  )
  (Stmt.assign
    (pattern (Patt.ident "client"))
    (Expr.module_access
      (Expr.malformed)
      (Expr.malformed)
    )
  )
  (Stmt.assign
    (pattern (Patt.ident "parser"))
    (Expr.malformed)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~

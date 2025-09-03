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
Nothing is named **Config.toString** in this scope.
Is there an **import** or **exposing** missing up-top?

**can_import_nested_modules.md:9:26:9:41:**
```roc
parseConfig = |settings| Config.toString(settings)
```
                         ^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **HttpAuth.login** in this scope.
Is there an **import** or **exposing** missing up-top?

**can_import_nested_modules.md:13:29:13:43:**
```roc
authenticate = |user, pass| HttpAuth.login(user, pass)
```
                            ^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_import_nested_modules.md:18:5:18:37:**
```roc
    Config.Parser.Advanced.parseWith(advancedConfig, input)
```
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **padLeft** in this scope.
Is there an **import** or **exposing** missing up-top?

**can_import_nested_modules.md:22:23:22:30:**
```roc
formatOutput = |text| padLeft(text, Config.defaultPadding)
```
                      ^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **Config.defaultPadding** in this scope.
Is there an **import** or **exposing** missing up-top?

**can_import_nested_modules.md:22:37:22:58:**
```roc
formatOutput = |text| padLeft(text, Config.defaultPadding)
```
                                    ^^^^^^^^^^^^^^^^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **HttpAuth.validate** in this scope.
Is there an **import** or **exposing** missing up-top?

**can_import_nested_modules.md:26:24:26:41:**
```roc
validateAuth = |creds| HttpAuth.validate(creds)
```
                       ^^^^^^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
  (Stmt.import)
  (Stmt.import)
  (Stmt.type_anno
    (name "parseConfig")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "parseConfig"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_anno
    (name "authenticate")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "authenticate"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_anno
    (name "processData")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "processData"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_anno
    (name "formatOutput")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "formatOutput"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_anno
    (name "validateAuth")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "validateAuth"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
~~~

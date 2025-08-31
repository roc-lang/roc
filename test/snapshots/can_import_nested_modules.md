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
NIL
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

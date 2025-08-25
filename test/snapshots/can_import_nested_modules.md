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
KwModule OpenSquare CloseSquare KwImport LowerIdent Dot UpperIdent Dot UpperIdent KwImport LowerIdent Dot UpperIdent Dot UpperIdent KwAs UpperIdent KwImport LowerIdent Dot UpperIdent Dot UpperIdent KwExposing OpenSquare LowerIdent CloseSquare LowerIdent OpColon UpperIdent Dot UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound LowerIdent OpColon UpperIdent Comma UpperIdent OpArrow UpperIdent Dot UpperIdent LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar UpperIdent Dot LowerIdent OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpColon UpperIdent Dot UpperIdent Dot UpperIdent Comma UpperIdent OpArrow UpperIdent OpenRound UpperIdent Comma UpperIdent Dot UpperIdent Dot UpperIdent CloseRound LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar UpperIdent Dot UpperIdent Dot UpperIdent Dot LowerIdent OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpenRound LowerIdent Comma UpperIdent Dot LowerIdent CloseRound LowerIdent OpColon UpperIdent Dot UpperIdent OpArrow UpperIdent OpenRound UpperIdent Dot UpperIdent Comma UpperIdent Dot UpperIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound ~~~
# PARSE
~~~clojure
(block
  (import
    (lc "json")
    (uc "Parser")
    (uc "Config")
  )
  (import
    (lc "http")
    (uc "Client")
    (uc "Auth")
    (uc "HttpAuth")
  )
  (import
    (lc "utils")
    (uc "String")
    (uc "Format")
    (lc "padLeft")
  )
  (binop_colon
    (lc "parseConfig")
    (binop_thin_arrow
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
    (binop_thin_arrow
      (uc "Str")
      (binop_thin_arrow
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
        (tuple_literal
          (lc "user")
          (lc "pass")
        )
      )
    )
  )
  (binop_colon
    (lc "processData")
    (binop_thin_arrow
      (binop_pipe
        (binop_pipe
          (uc "Config")
          (uc "Parser")
        )
        (uc "Advanced")
      )
      (binop_thin_arrow
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
        (tuple_literal
          (lc "advancedConfig")
          (lc "input")
        )
      )
    )
  )
  (binop_colon
    (lc "formatOutput")
    (binop_thin_arrow
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
    (binop_thin_arrow
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

import json.Parser.Config
import http.Client.Auth as HttpAuth
import utils.String.Format exposing [padLeft]
parseConfig: (Config.Settings -> Str)
parseConfig = \settings -> Config.toString(settings)

# Test multi-level value qualification
authenticate: (Str -> (Str -> HttpAuth.Token))
authenticate = \(
	user,
	pass
) -> HttpAuth.login((user, pass))
processData: (Config.Parser | Advanced -> (Str -> Result((Str, Config.Parser | Error))))
processData = \(
	advancedConfig,
	input
) -> (Config.Parser | Advanced) | .parseWith((advancedConfig, input))
formatOutput: (Str -> Str)
formatOutput = \text -> padLeft((text, Config.defaultPadding))
validateAuth: (HttpAuth.Credentials -> Result((HttpAuth.Token, HttpAuth.Error)))
validateAuth = \creds -> HttpAuth.validate(creds)
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 3:1 to 3:26

**Unsupported Node**
at 4:1 to 4:36

**Unsupported Node**
at 5:1 to 5:45

**Unsupported Node**
at 8:15 to 8:37

**Unsupported Node**
at 9:15 to 9:26

**Unsupported Node**
at 12:16 to 12:41

**Unsupported Node**
at 13:16 to 13:29

**Unsupported Node**
at 16:15 to 16:78

**Unsupported Node**
at 17:15 to 18:5

**Unsupported Node**
at 21:16 to 21:26

**Unsupported Node**
at 22:16 to 22:23

**Unsupported Node**
at 25:16 to 25:78

**Unsupported Node**
at 26:16 to 26:24

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "parseConfig")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "authenticate")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "processData")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "formatOutput")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "validateAuth")
    (Expr.malformed)
  )
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
~~~

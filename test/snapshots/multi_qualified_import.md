# META
~~~ini
description=Test multi-level qualified imports and type annotations
type=file
~~~
# SOURCE
~~~roc
module [json_encoder]

import json.Core.Utf8 exposing [Encoder]

json_encoder : Encoder
json_encoder = Json.Core.Utf8.defaultEncoder

# Test with qualified type in annotation
process : json.Core.Utf8.Encoder -> Str
process = |encoder| "processing"

# Test with multiple qualifiers
data : json.Core.Utf8.EncodedData
data = json.Core.Utf8.encode("hello")
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare KwImport LowerIdent Dot UpperIdent Dot UpperIdent KwExposing OpenSquare UpperIdent CloseSquare LowerIdent OpColon UpperIdent LowerIdent OpAssign UpperIdent Dot UpperIdent Dot UpperIdent Dot LowerIdent LowerIdent OpColon LowerIdent Dot UpperIdent Dot UpperIdent Dot UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar String LowerIdent OpColon LowerIdent Dot UpperIdent Dot UpperIdent Dot UpperIdent LowerIdent OpAssign LowerIdent Dot UpperIdent Dot UpperIdent Dot LowerIdent OpenRound String CloseRound ~~~
# PARSE
~~~clojure
(block
  (import
    (lc "json")
    (uc "Core")
    (uc "Utf8")
    (uc "Encoder")
  )
  (binop_colon
    (lc "json_encoder")
    (uc "Encoder")
  )
  (binop_equals
    (lc "json_encoder")
    (binop_pipe
      (binop_pipe
        (binop_pipe
          (uc "Json")
          (uc "Core")
        )
        (uc "Utf8")
      )
      (dot_lc "defaultEncoder")
    )
  )
  (binop_colon
    (lc "process")
    (binop_thin_arrow
      (binop_pipe
        (binop_pipe
          (binop_pipe
            (lc "json")
            (uc "Core")
          )
          (uc "Utf8")
        )
        (uc "Encoder")
      )
      (uc "Str")
    )
  )
  (binop_equals
    (lc "process")
    (lambda
      (body
        (str_literal_big "processing")
      )
      (args
        (lc "encoder")
      )
    )
  )
  (binop_colon
    (lc "data")
    (binop_pipe
      (binop_pipe
        (binop_pipe
          (lc "json")
          (uc "Core")
        )
        (uc "Utf8")
      )
      (uc "EncodedData")
    )
  )
  (binop_equals
    (lc "data")
    (apply_anon
      (binop_pipe
        (binop_pipe
          (binop_pipe
            (lc "json")
            (uc "Core")
          )
          (uc "Utf8")
        )
        (dot_lc "encode")
      )
      (str_literal_big "hello")
    )
  )
)
~~~
# FORMATTED
~~~roc
module [
	json_encoder,
]

import json exposing [Core, Utf8, Encoder]
json_encoder: Encoder
json_encoder = ((Json.Core) | Utf8) | .defaultEncoder

# Test with qualified type in annotation
process: (((json.Core) | Utf8) | Encoder -> Str)
process = \encoder -> "processing"

# Test with multiple qualifiers
data: ((json.Core) | Utf8) | EncodedData
data = ((json.Core) | Utf8) | .encode("hello")
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 3:1 to 3:40

**Unsupported Node**
at 6:16 to 6:20

**Unsupported Node**
at 6:20 to 6:24

**Unsupported Node**
at 6:25 to 6:29

**Unsupported Node**
at 9:11 to 9:40

**Unsupported Node**
at 10:11 to 10:21

**Unsupported Node**
at 13:12 to 13:16

**Unsupported Node**
at 13:17 to 13:21

**Unsupported Node**
at 14:12 to 14:16

**Unsupported Node**
at 14:17 to 14:21

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "json_encoder")
    (Expr.apply_tag)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "process")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "data")
    (Expr.lambda)
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

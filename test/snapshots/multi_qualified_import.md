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

import json.Core.Utf8 exposing [Encoder]
json_encoder : Encoder
json_encoder = (Json.Core | Utf8) | .defaultEncoder
process : (json.Core | Utf8) | Encoder -> Str
process = \encoder -> "processing"

# Test with multiple qualifiers
data : (json.Core | Utf8) | EncodedData
data = (json.Core | Utf8) | .encode("hello")
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_plus)
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

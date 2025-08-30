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
(module-header
  (exposes
    (lc "json_encoder")
))
~~~
# FORMATTED
~~~roc
module [json_encoder]

import json.Core | Utf8 exposing [Encoder]
json_encoder : Encoder
json_encoder = (Json.Core | Utf8 | .defaultEncoder)
process : json.Core | Utf8 | Encoder -> Str
process = |encoder| "processing"
data : json.Core | Utf8 | EncodedData
data = json.Core | Utf8 | .encode("hello")
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 3:1 to 3:41

**Unsupported Node**
at 6:16 to 6:20

**Unsupported Node**
at 6:20 to 6:25

**Unsupported Node**
at 6:25 to 6:30

**Unsupported Node**
at 9:15 to 9:20

**Unsupported Node**
at 9:20 to 9:25

**Unsupported Node**
at 13:12 to 13:17

**Unsupported Node**
at 13:17 to 13:22

**Unsupported Node**
at 14:12 to 14:17

**Unsupported Node**
at 14:17 to 14:22

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "json_encoder")
    (Expr.apply_tag)
  )
  (Expr.binop_equals
    (Expr.lookup "json_encoder")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.lookup "process")
    (Expr.binop_thin_arrow
      (Expr.lambda)
      (Expr.apply_tag)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "process")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.lookup "data")
    (Expr.lambda)
  )
  (Expr.binop_equals
    (Expr.lookup "data")
    (Expr.apply_ident)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
json_encoder : _a
process : _a
data : _a
~~~

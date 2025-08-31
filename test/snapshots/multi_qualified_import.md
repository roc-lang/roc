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
KwModule OpenSquare LowerIdent CloseSquare BlankLine KwImport LowerIdent Dot UpperIdent Dot UpperIdent KwExposing OpenSquare UpperIdent CloseSquare BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign UpperIdent Dot UpperIdent Dot UpperIdent Dot LowerIdent BlankLine LineComment LowerIdent OpColon LowerIdent Dot UpperIdent Dot UpperIdent Dot UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar String BlankLine LineComment LowerIdent OpColon LowerIdent Dot UpperIdent Dot UpperIdent Dot UpperIdent LowerIdent OpAssign LowerIdent Dot UpperIdent Dot UpperIdent Dot LowerIdent OpenRound String CloseRound ~~~
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
data = json.Core | Utf8 | .encode("hello")# Test with qualified type in annotation
# Test with multiple qualifiers
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**multi_qualified_import.md:3:1:3:41:**
```roc
import json.Core.Utf8 exposing [Encoder]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**multi_qualified_import.md:6:16:6:20:**
```roc
json_encoder = Json.Core.Utf8.defaultEncoder
```
               ^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**multi_qualified_import.md:6:20:6:25:**
```roc
json_encoder = Json.Core.Utf8.defaultEncoder
```
                   ^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**multi_qualified_import.md:6:25:6:30:**
```roc
json_encoder = Json.Core.Utf8.defaultEncoder
```
                        ^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**multi_qualified_import.md:9:15:9:20:**
```roc
process : json.Core.Utf8.Encoder -> Str
```
              ^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**multi_qualified_import.md:9:20:9:25:**
```roc
process : json.Core.Utf8.Encoder -> Str
```
                   ^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**multi_qualified_import.md:13:12:13:17:**
```roc
data : json.Core.Utf8.EncodedData
```
           ^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**multi_qualified_import.md:13:17:13:22:**
```roc
data : json.Core.Utf8.EncodedData
```
                ^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**multi_qualified_import.md:14:12:14:17:**
```roc
data = json.Core.Utf8.encode("hello")
```
           ^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**multi_qualified_import.md:14:17:14:22:**
```roc
data = json.Core.Utf8.encode("hello")
```
                ^^^^^


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

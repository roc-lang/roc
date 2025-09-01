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

# Test with qualified type in annotation
process : json.Core | Utf8 | Encoder -> Str
process = |encoder| "processing"

# Test with multiple qualifiers
data : json.Core | Utf8 | EncodedData
data = json.Core | Utf8 | .encode("hello")
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**multi_qualified_import.md:6:16:6:45:**
```roc
json_encoder = Json.Core.Utf8.defaultEncoder
```
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


**UNUSED VARIABLE**
Variable **encoder** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_encoder` to suppress this warning.
The unused variable is declared here:

**multi_qualified_import.md:10:12:10:19:**
```roc
process = |encoder| "processing"
```
           ^^^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**multi_qualified_import.md:14:8:14:29:**
```roc
data = json.Core.Utf8.encode("hello")
```
       ^^^^^^^^^^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
  (Stmt.type_anno
    (name "json_encoder")
    (type uc)
  )
  (Stmt.assign
    (pattern (Patt.ident "json_encoder"))
    (Expr.malformed)
  )
  (Stmt.type_anno
    (name "process")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "process"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.type_anno
    (name "data")
    (type binop_pipe)
  )
  (Stmt.assign
    (pattern (Patt.ident "data"))
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
~~~

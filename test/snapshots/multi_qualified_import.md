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
(block
  (import
    (binop_exposing
      (binop_dot
        (binop_dot
          (lc "json")
          (uc "Core")
        )
        (uc "Utf8")
      )
      (list_literal
        (uc "Encoder")
      )
    )
  )
  (binop_colon
    (lc "json_encoder")
    (uc "Encoder")
  )
  (binop_equals
    (lc "json_encoder")
    (binop_dot
      (binop_dot
        (binop_dot
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
    (binop_arrow_call
      (binop_dot
        (binop_dot
          (binop_dot
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
    (binop_dot
      (binop_dot
        (binop_dot
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
      (binop_dot
        (binop_dot
          (binop_dot
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
module [json_encoder]

import json.Core.Utf8 exposing [Encoder]
json_encoder : Encoder
json_encoder = (Json.Core.Utf8..defaultEncoder)
# Test with qualified type in annotation
process : json.Core.Utf8.Encoder -> Str
process = |encoder| "processing"
# Test with multiple qualifiers
data : json.Core.Utf8.EncodedData
data = json.Core.Utf8..encode("hello")
~~~
# EXPECTED
PARSE ERROR - multi_qualified_import.md:3:17:3:22
PARSE ERROR - multi_qualified_import.md:3:23:3:31
PARSE ERROR - multi_qualified_import.md:3:32:3:33
PARSE ERROR - multi_qualified_import.md:3:40:3:41
PARSE ERROR - multi_qualified_import.md:14:12:14:17
PARSE ERROR - multi_qualified_import.md:14:17:14:22
PARSE ERROR - multi_qualified_import.md:14:22:14:29
PARSE ERROR - multi_qualified_import.md:14:29:14:30
PARSE ERROR - multi_qualified_import.md:14:30:14:31
PARSE ERROR - multi_qualified_import.md:14:31:14:36
PARSE ERROR - multi_qualified_import.md:14:36:14:37
PARSE ERROR - multi_qualified_import.md:14:37:14:38
MODULE NOT FOUND - multi_qualified_import.md:3:1:3:17
UNDECLARED TYPE - multi_qualified_import.md:5:16:5:23
UNDEFINED VARIABLE - multi_qualified_import.md:6:16:6:45
MODULE NOT IMPORTED - multi_qualified_import.md:9:11:9:33
UNUSED VARIABLE - multi_qualified_import.md:10:12:10:19
MODULE NOT IMPORTED - multi_qualified_import.md:13:8:13:34
UNDEFINED VARIABLE - multi_qualified_import.md:14:8:14:12
# PROBLEMS
**SHADOWING**
This definition shadows an existing one.

**multi_qualified_import.md:6:1:6:13:**
```roc
json_encoder = Json.Core.Utf8.defaultEncoder
```
^^^^^^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**multi_qualified_import.md:9:1:9:8:**
```roc
process : json.Core.Utf8.Encoder -> Str
```
^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**multi_qualified_import.md:10:1:10:8:**
```roc
process = |encoder| "processing"
```
^^^^^^^


**SHADOWING**
This definition shadows an existing one.

**multi_qualified_import.md:13:1:13:5:**
```roc
data : json.Core.Utf8.EncodedData
```
^^^^


**UNDEFINED VARIABLE**
Nothing is named **json** in this scope.
Is there an **import** or **exposing** missing up-top?

**multi_qualified_import.md:14:8:14:12:**
```roc
data = json.Core.Utf8.encode("hello")
```
       ^^^^


**SHADOWING**
This definition shadows an existing one.

**multi_qualified_import.md:14:1:14:5:**
```roc
data = json.Core.Utf8.encode("hello")
```
^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "json_encoder"))
    (type type_12)
  )
  (Stmt.assign
    (pattern (Patt.ident "json_encoder"))
    (Expr.record_access)
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "process"))
    (type type_32)
  )
  (Stmt.assign
    (pattern (Patt.ident "process"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "data"))
    (type type_46)
  )
  (Stmt.assign
    (pattern (Patt.ident "data"))
    (Expr.fn_call)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 64
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 _)
(var #11 _)
(var #12 _)
(var #13 _)
(var #14 -> #21)
(var #15 _)
(var #16 _)
(var #17 -> #60)
(var #18 _)
(var #19 _)
(var #20 _)
(var #21 _)
(var #22 _)
(var #23 _)
(var #24 _)
(var #25 _)
(var #26 _)
(var #27 _)
(var #28 _)
(var #29 _)
(var #30 _)
(var #31 _)
(var #32 _)
(var #33 _)
(var #34 -> #62)
(var #35 _)
(var #36 Str)
(var #37 -> #62)
(var #38 _)
(var #39 _)
(var #40 _)
(var #41 _)
(var #42 _)
(var #43 _)
(var #44 _)
(var #45 _)
(var #46 _)
(var #47 _)
(var #48 -> #57)
(var #49 _)
(var #50 _)
(var #51 _)
(var #52 _)
(var #53 _)
(var #54 _)
(var #55 -> #63)
(var #56 Str)
(var #57 _)
(var #58 _)
(var #59 _)
(var #60 _)
(var #61 _)
(var #62 fn_pure)
(var #63 fn_pure)
~~~
# TYPES
~~~roc
data : _a
json_encoder : _a
encoder : _a
process : _arg -> Str
~~~

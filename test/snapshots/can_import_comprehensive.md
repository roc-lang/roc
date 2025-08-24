# META
~~~ini
description=Comprehensive import test with various module access patterns
type=file
~~~
# SOURCE
~~~roc
module []

import json.Json
import http.Client as Http exposing [get, post]
import utils.String as Str

main = {
    client = Http.get
    parser = Json.utf8
    helper = Str.trim

    # Test direct module access
    result1 = Json.parse

    # Test aliased module access
    result2 = Http.post

    # Test exposed items (should work without module prefix)
    result3 = get
    result4 = post

    # Test multiple qualified access
    combined = Str.concat

    (
        client,
        parser,
        helper,
        result1,
        result2,
        result3,
        result4,
        combined,
    )
}
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare KwImport LowerIdent Dot UpperIdent KwImport LowerIdent Dot UpperIdent KwAs UpperIdent KwExposing OpenSquare LowerIdent Comma LowerIdent CloseSquare KwImport LowerIdent Dot UpperIdent KwAs UpperIdent LowerIdent OpAssign OpenCurly LowerIdent OpAssign UpperIdent Dot LowerIdent LowerIdent OpAssign UpperIdent Dot LowerIdent LowerIdent OpAssign UpperIdent Dot LowerIdent LowerIdent OpAssign UpperIdent Dot LowerIdent LowerIdent OpAssign UpperIdent Dot LowerIdent LowerIdent OpAssign LowerIdent LowerIdent OpAssign LowerIdent LowerIdent OpAssign UpperIdent Dot LowerIdent OpenRound LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent Comma CloseRound CloseCurly ~~~
# PARSE
~~~clojure
(block
  (import
    (lc "json")
    (uc "Json")
  )
  (import
    (lc "http")
    (uc "Client")
    (uc "Http")
    (lc "get")
    (lc "post")
  )
  (import
    (lc "utils")
    (uc "String")
    (uc "Str")
  )
  (binop_equals
    (lc "main")
    (block
      (binop_equals
        (lc "client")
        (binop_pipe
          (uc "Http")
          (dot_lc "get")
        )
      )
      (binop_equals
        (lc "parser")
        (binop_pipe
          (uc "Json")
          (dot_lc "utf8")
        )
      )
      (binop_equals
        (lc "helper")
        (binop_pipe
          (uc "Str")
          (dot_lc "trim")
        )
      )
      (binop_equals
        (lc "result1")
        (binop_pipe
          (uc "Json")
          (dot_lc "parse")
        )
      )
      (binop_equals
        (lc "result2")
        (binop_pipe
          (uc "Http")
          (dot_lc "post")
        )
      )
      (binop_equals
        (lc "result3")
        (lc "get")
      )
      (binop_equals
        (lc "result4")
        (lc "post")
      )
      (binop_equals
        (lc "combined")
        (apply_anon
          (binop_pipe
            (uc "Str")
            (dot_lc "concat")
          )
          (tuple_literal
            (lc "client")
            (lc "parser")
            (lc "helper")
            (lc "result1")
            (lc "result2")
            (lc "result3")
            (lc "result4")
            (lc "combined")
            (malformed malformed:expr_unexpected_token)
          )
        )
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
MODULE NOT FOUND - can_import_comprehensive.md:3:1:3:17
MODULE NOT FOUND - can_import_comprehensive.md:4:1:4:48
MODULE NOT FOUND - can_import_comprehensive.md:5:1:5:27
# PROBLEMS
**Parse Error**
at 34:5 to 34:5

**Parse Error**
at 23:19 to 35:1

**Unsupported Node**
at 3:1 to 3:17

**Unsupported Node**
at 4:1 to 4:47

**Unsupported Node**
at 5:1 to 5:27

**Unsupported Node**
at 8:14 to 8:18

**Unsupported Node**
at 9:14 to 9:18

**Unsupported Node**
at 10:14 to 10:17

**Unsupported Node**
at 13:15 to 13:19

**Unsupported Node**
at 16:15 to 16:19

**Unsupported Node**
at 23:16 to 23:19

**Unsupported Node**
at 34:5 to 34:5

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
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

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
module []

import json.Json
import http.Client as Http exposing [get, post]
import utils.String as Str

main = {
	client = Http.get
	parser = Json.utf8
	helper = Str.trim
	result1 = Json.parse
	result2 = Http.post
	result3 = get
	result4 = post
	

# Test multiple qualified access
combined = Str.concat((client, parser, helper, result1, result2, result3, result4, combined))
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 34:5 to 34:5

**Parse Error**
at 23:19 to 35:1

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_plus)
  (Expr.binop_plus)
  (Expr.binop_plus)
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

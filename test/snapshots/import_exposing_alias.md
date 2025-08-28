# META
~~~ini
description=Import with exposing clause using aliases
type=file
~~~
# SOURCE
~~~roc
module [main]

import json.Json exposing [decode as fromJson, encode as toJson]

main = {
	data = { name: "Bob", age: 25 }
	encoded = toJson(data)
	decoded = fromJson(encoded)
	decoded
}
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare KwImport LowerIdent Dot UpperIdent KwExposing OpenSquare LowerIdent KwAs LowerIdent Comma LowerIdent KwAs LowerIdent CloseSquare LowerIdent OpAssign OpenCurly LowerIdent OpAssign OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon Int CloseCurly LowerIdent OpAssign LowerIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign LowerIdent OpenRound LowerIdent CloseRound LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(block
  (import
    (binop_exposing
      (binop_pipe
        (lc "json")
        (uc "Json")
      )
      (list_literal
        (lc "decode")
      )
    )
  )
  (malformed malformed:expr_unexpected_token)
  (lc "fromJson")
  (malformed malformed:expr_unexpected_token)
  (lc "encode")
  (malformed malformed:expr_unexpected_token)
  (lc "toJson")
  (malformed malformed:expr_unexpected_token)
  (binop_equals
    (lc "main")
    (block
      (binop_equals
        (lc "data")
        (record_literal
          (binop_colon
            (lc "name")
            (str_literal_small "Bob")
          )
          (binop_colon
            (lc "age")
            (num_literal_i32 25)
          )
        )
      )
      (binop_equals
        (lc "encoded")
        (apply_lc
          (lc "toJson")
          (lc "data")
        )
      )
      (binop_equals
        (lc "decoded")
        (apply_lc
          (lc "fromJson")
          (lc "encoded")
        )
      )
      (lc "decoded")
    )
  )
)
~~~
# FORMATTED
~~~roc
module [main]

import json.Json exposing [decode]
fromJson
encode
toJson
main = {
	data = { name : "Bob", age : 25 }
	encoded = toJson(data)
	decoded = fromJson(encoded)
	decoded
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 3:35 to 3:35

**Parse Error**
at 3:46 to 3:46

**Parse Error**
at 3:55 to 3:55

**Parse Error**
at 3:64 to 3:64

**Unsupported Node**
at 3:1 to 3:34

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.lookup "fromJson")
  (Expr.malformed)
  (Expr.lookup "encode")
  (Expr.malformed)
  (Expr.lookup "toJson")
  (Expr.malformed)
  (Expr.binop_equals
    (Expr.lookup "main")
    (Expr.block
      (Expr.binop_equals
        (Expr.lookup "data")
        (Expr.record_literal
          (Expr.binop_colon
            (Expr.lookup "name")
            (Expr.str_literal_small)
          )
          (Expr.binop_colon
            (Expr.lookup "age")
            (Expr.num_literal_i32 25)
          )
        )
      )
      (Expr.binop_equals
        (Expr.lookup "encoded")
        (Expr.apply_ident)
      )
      (Expr.binop_equals
        (Expr.lookup "decoded")
        (Expr.apply_ident)
      )
      (Expr.lookup "decoded")
    )
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
main : _a
~~~

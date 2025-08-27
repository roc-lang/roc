# META
~~~ini
description=Import with exposing clause and usage of exposed items
type=file
~~~
# SOURCE
~~~roc
module [main]

import json.Json exposing [decode, encode]

main = {
    data = { name: "Alice", age: 30 }
    encoded = encode(data)
    decoded = decode(encoded)
    decoded
}
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare KwImport LowerIdent Dot UpperIdent KwExposing OpenSquare LowerIdent Comma LowerIdent CloseSquare LowerIdent OpAssign OpenCurly LowerIdent OpAssign OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon Int CloseCurly LowerIdent OpAssign LowerIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign LowerIdent OpenRound LowerIdent CloseRound LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(block
  (import
    (lc "json")
    (uc "Json")
    (lc "decode")
    (lc "encode")
  )
  (binop_equals
    (lc "main")
    (block
      (binop_equals
        (lc "data")
        (record_literal
          (binop_colon
            (lc "name")
            (str_literal_big "Alice")
          )
          (binop_colon
            (lc "age")
            (num_literal_i32 30)
          )
        )
      )
      (binop_equals
        (lc "encoded")
        (apply_lc
          (lc "encode")
          (lc "data")
        )
      )
      (binop_equals
        (lc "decoded")
        (apply_lc
          (lc "decode")
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

import json.Json exposing [decode, encode]
main = {
	data = { name : "Alice", age : 30 }
	encoded = encode(data)
	decoded = decode(encoded)
	decoded
}
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
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~

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
        (block
          (binop_colon
            (lc "name")
            (binop_colon
              (tuple_literal
                (str_literal_big "Alice")
                (lc "age")
              )
              (num_literal_i32 30)
            )
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
module [
	main,
]

import json exposing [Json, decode, encode]
main = {
	data = {
		name: (("Alice", age): 30)
	}
	encoded = encode(data)
	decoded = decode(encoded)
	decoded
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 3:1 to 3:42

**Unsupported Node**
at 6:32 to 6:33

# CANONICALIZE
~~~clojure
(Expr.block
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

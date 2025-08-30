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
(module-header
  (exposes
    (lc "main")
))
~~~
# FORMATTED
~~~roc
module [main]

import json.Json exposing [decode]
as fromJson, encode
as toJson]

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
at 3:35 to 3:38

**Parse Error**
at 3:46 to 3:48

**Parse Error**
at 3:55 to 3:58

**Parse Error**
at 3:64 to 5:1

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

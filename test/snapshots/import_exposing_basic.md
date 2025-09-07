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
KwModule OpenSquare LowerIdent CloseSquare BlankLine KwImport LowerIdent Dot UpperIdent KwExposing OpenSquare LowerIdent Comma LowerIdent CloseSquare BlankLine LowerIdent OpAssign OpenCurly LowerIdent OpAssign OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon Int CloseCurly LowerIdent OpAssign LowerIdent OpenRound LowerIdent CloseRound LowerIdent OpAssign LowerIdent OpenRound LowerIdent CloseRound LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "main")
))
(block
  (import
    (binop_exposing
      (binop_dot
        (lc "json")
        (uc "Json")
      )
      (list_literal
        (lc "decode")
        (lc "encode")
      )
    )
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
	data = { name: "Alice", age: 30 }
	encoded = encode(data)
	decoded = decode(encoded)
	decoded
}
~~~
# EXPECTED
MODULE NOT FOUND - import_exposing_basic.md:3:1:3:43
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **encode** in this scope.
Is there an **import** or **exposing** missing up-top?

**import_exposing_basic.md:7:15:7:21:**
```roc
    encoded = encode(data)
```
              ^^^^^^


**UNDEFINED VARIABLE**
Nothing is named **decode** in this scope.
Is there an **import** or **exposing** missing up-top?

**import_exposing_basic.md:8:15:8:21:**
```roc
    decoded = decode(encoded)
```
              ^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.import)
  (Stmt.assign
    (pattern (Patt.ident "main"))
    (Expr.block
      (Stmt.assign
        (pattern (Patt.ident "data"))
        (Expr.record_literal
          (Expr.record_field
            (Expr.malformed)
            (Expr.str_literal_big)
          )
          (Expr.record_field
            (Expr.malformed)
            (Expr.num_literal_i32 30)
          )
        )
      )
      (Stmt.assign
        (pattern (Patt.ident "encoded"))
        (Expr.fn_call)
      )
      (Stmt.assign
        (pattern (Patt.ident "decoded"))
        (Expr.fn_call)
      )
      (Expr.lookup "decoded")
    )
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 38
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
(var #10 -> #31)
(var #11 -> #35)
(var #12 _)
(var #13 Str)
(var #14 _)
(var #15 _)
(var #16 Num *)
(var #17 _)
(var #18 -> #35)
(var #19 _)
(var #20 -> #23)
(var #21 -> #36)
(var #22 _)
(var #23 _)
(var #24 _)
(var #25 -> #28)
(var #26 -> #37)
(var #27 _)
(var #28 _)
(var #29 _)
(var #30 _)
(var #31 _)
(var #32 _)
(var #33 _)
(var #34 {})
(var #35 record)
(var #36 fn_pure)
(var #37 fn_pure)
~~~
# TYPES
~~~roc
~~~
